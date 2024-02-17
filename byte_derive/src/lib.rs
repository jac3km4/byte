use quote::ToTokens;
use syn::{punctuated::Punctuated, spanned::Spanned};

fn impl_data_read(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let mut lifetimes = input.generics.lifetimes();
    let (_, ty_generics, where_clause) = input.generics.split_for_impl();
    let mut generics = generics_with_ctx(&input.generics);
    // use the first lifetime parameter as the input lifetime if there is one
    // otherwise create a new lifetime parameter and add it to the generics
    let input_lt = if let Some(lt) = lifetimes.next() {
        lt.clone()
    } else {
        let lt = syn::LifetimeParam::new(syn::Lifetime::new("'input", generics.span()));
        generics
            .params
            .push(syn::GenericParam::Lifetime(lt.clone()));
        lt
    };
    let (impl_generics, _, _) = generics.split_for_impl();

    if lifetimes.next().is_some() {
        return Err(syn::Error::new(
            generics.span(),
            "only one lifetime parameter is allowed for TryRead derive",
        ));
    }

    let body = match &input.data {
        syn::Data::Struct(data) => impl_read_fields(name.into_token_stream(), &data.fields),
        syn::Data::Enum(data) => {
            let tag_type = ContainerAttributes::parse(&input.attrs)?
                .tag_type
                .ok_or_else(|| {
                    syn::Error::new(input.span(), "tag_type attribute is required for enums")
                })?;

            let variants = data
                .variants
                .iter()
                .map(|variant| {
                    let attrs = VariantAttributes::parse(&variant.attrs)?;
                    let tag = attrs.tag.ok_or_else(|| {
                        syn::Error::new(input.span(), "tag attribute is required for enum variants")
                    })?;
                    let variant_name = &variant.ident;
                    let body =
                        impl_read_fields(quote::quote!(#name::#variant_name), &variant.fields);
                    Ok(quote::quote! { #tag => #body })
                })
                .collect::<syn::Result<Vec<_>>>()?;

            quote::quote! {
                match ::byte::BytesExt::read::<#tag_type>(bytes, __offset, ctx)? {
                    #(#variants)*
                    _ => return Err(::byte::Error::BadInput {
                        err: stringify!(unknown tag #name),
                    }),
                }
            }
        }
        _ => {
            return Err(syn::Error::new(
                input.span(),
                "TryRead can only be derived for structs",
            ))
        }
    };

    let predicates = where_clause.map(|w| &w.predicates);
    let constraints = generic_constraints(
        &input.generics,
        quote::quote!(::byte::TryRead<#input_lt, __Ctx>),
    );
    Ok(quote::quote! {
        impl #impl_generics ::byte::TryRead<#input_lt, __Ctx> for #name #ty_generics
            where
                __Ctx: ::byte::ctx::Endianess,
                #( #constraints, )*
                #predicates {

            #[allow(unused_variables)]
            fn try_read(bytes: & #input_lt [u8], ctx: __Ctx) -> ::byte::Result<(Self, usize)> {
                let __offset = &mut 0;
                let result = #body;
                Ok((result, *__offset))
            }
        }
    })
}

fn impl_read_fields(
    name: proc_macro2::TokenStream,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    let it = fields.into_iter();
    let field_names = it.clone().enumerate().map(|(i, field)| {
        let borrowed_name = field
            .ident
            .clone()
            .unwrap_or_else(|| syn::Ident::new(&format!("_{i}"), field.span()));
        let owned_name = quote::format_ident!("__owned_{i}");
        (owned_name, borrowed_name)
    });

    let field_reads = it
        .zip(field_names.clone())
        .map(|(field, (owned_name, borrowed_name))| {
            let attrs = match FieldAttributes::parse(&field.attrs) {
                Ok(attrs) => attrs,
                Err(err) => return err.to_compile_error(),
            };

            let ctx = attrs
                .ctx
                .map_or_else(|| quote::quote!(ctx), |ctx| quote::quote!(#ctx));
            let ty = &field.ty;
            let read = if attrs.skip {
                quote::quote!(<#ty>::default())
            } else if let Some(skip_if) = attrs.skip_if {
                quote::quote! {
                    if #skip_if {
                        <#ty>::default()
                    } else {
                        ::byte::BytesExt::read::<#ty>(bytes, __offset, #ctx)?
                    }
                }
            } else {
                quote::quote!(::byte::BytesExt::read::<#ty>(bytes, __offset, #ctx)?)
            };
            quote::quote! {
                let #owned_name: #ty = #read;
                let #borrowed_name: &#ty = &#owned_name;
            }
        });

    let result = match fields {
        syn::Fields::Named(_) => {
            let it = field_names
                .map(|(owned_name, borrowed_name)| quote::quote!(#borrowed_name: #owned_name));
            quote::quote!(#name { #(#it),* })
        }
        syn::Fields::Unnamed(_) => {
            let it = field_names.map(|(owned_name, _)| owned_name);
            quote::quote!(#name(#(#it),*))
        }
        syn::Fields::Unit => quote::quote!(#name),
    };
    quote::quote! {{
        #(#field_reads)*
        #result
    }}
}

fn impl_data_write(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let (_, ty_generics, where_clause) = input.generics.split_for_impl();
    let generics = generics_with_ctx(&input.generics);
    let (impl_generics, _, _) = generics.split_for_impl();
    let container_attrs = ContainerAttributes::parse(&input.attrs)?;
    let name = &input.ident;

    let body = match &input.data {
        syn::Data::Struct(data) => {
            let (extract_fields, body) =
                impl_write_fields(input.ident.clone().into_token_stream(), &data.fields);
            quote::quote! {{
                let #extract_fields = self;
                #body
            }}
        }
        syn::Data::Enum(data) => {
            let tag_type = container_attrs.tag_type.ok_or_else(|| {
                syn::Error::new(input.span(), "tag_type attribute is required for enums")
            })?;

            let variants = data
                .variants
                .iter()
                .map(|variant| {
                    let attrs = VariantAttributes::parse(&variant.attrs)?;
                    let tag = attrs.tag.ok_or_else(|| {
                        syn::Error::new(input.span(), "tag attribute is required for enum variants")
                    })?;
                    let variant_name = &variant.ident;
                    let (extract_fields, body) =
                        impl_write_fields(quote::quote!(#name::#variant_name), &variant.fields);
                    Ok(quote::quote! {
                        #extract_fields => {
                            ::byte::BytesExt::write::<#tag_type>(bytes, __offset, &#tag, ctx)?;
                            #body
                        }
                    })
                })
                .collect::<syn::Result<Vec<_>>>()?;

            quote::quote! {
                match self {
                    #(#variants)*
                }
            }
        }
        _ => {
            return Err(syn::Error::new(
                input.span(),
                "TryWrite can only be derived for structs",
            ))
        }
    };
    let predicates = where_clause.map(|w| &w.predicates);
    let constraints = generic_constraints(&input.generics, quote::quote!(::byte::TryWrite<__Ctx>));
    Ok(quote::quote! {
        impl #impl_generics ::byte::TryWrite<__Ctx> for #name #ty_generics
            where
                __Ctx: ::byte::ctx::Endianess,
                #( #constraints, )*
                #predicates {

            #[allow(unused_variables)]
            fn try_write(&self, bytes: &mut [u8], ctx: __Ctx) -> ::byte::Result<usize> {
                let __offset = &mut 0;
                #body
                Ok(*__offset)
            }
        }
    })
}

fn impl_write_fields(
    name: proc_macro2::TokenStream,
    fields: &syn::Fields,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let it = fields.into_iter();
    let field_names = it.clone().enumerate().map(|(i, field)| {
        field
            .ident
            .clone()
            .unwrap_or_else(|| syn::Ident::new(&format!("_{i}"), field.span()))
    });

    let field_writes = it.zip(field_names.clone()).map(|(field, name)| {
        let attrs = match FieldAttributes::parse(&field.attrs) {
            Ok(attrs) => attrs,
            Err(err) => return err.to_compile_error(),
        };
        let ctx = attrs
            .ctx
            .map_or_else(|| quote::quote!(ctx), |ctx| quote::quote!(#ctx));
        if attrs.skip {
            quote::quote! {}
        } else if let Some(skip_if) = attrs.skip_if {
            quote::quote! {
                if !#skip_if {
                    ::byte::BytesExt::write(bytes, __offset, #name, #ctx)?;
                }
            }
        } else {
            quote::quote!(::byte::BytesExt::write(bytes, __offset, #name, #ctx)?;)
        }
    });

    let extract_fields = match fields {
        syn::Fields::Named(_) => quote::quote!(#name { #(#field_names),* }),
        syn::Fields::Unnamed(_) => quote::quote!(#name ( #(#field_names),* )),
        syn::Fields::Unit => quote::quote!(#name),
    };

    let body = quote::quote! {{#(#field_writes)*}};

    (extract_fields, body)
}

fn impl_data_measure(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let (_, ty_generics, where_clause) = input.generics.split_for_impl();
    let generics = generics_with_ctx(&input.generics);
    let (impl_generics, _, _) = generics.split_for_impl();
    let container_attrs = ContainerAttributes::parse(&input.attrs)?;
    let name = &input.ident;

    let body = match &input.data {
        syn::Data::Struct(data) => {
            let (extract_fields, body) =
                impl_measure_fields(input.ident.clone().into_token_stream(), &data.fields);
            quote::quote! {{
                let #extract_fields = self;
                #body
            }}
        }
        syn::Data::Enum(data) => {
            let tag_type = container_attrs.tag_type.ok_or_else(|| {
                syn::Error::new(input.span(), "tag_type attribute is required for enums")
            })?;

            let variants = data
                .variants
                .iter()
                .map(|variant| {
                    let attrs = VariantAttributes::parse(&variant.attrs)?;
                    let tag = attrs.tag.ok_or_else(|| {
                        syn::Error::new(
                            variant.span(),
                            "tag attribute is required for enum variants",
                        )
                    })?;
                    let variant_name = &variant.ident;
                    let (extract_fields, body) =
                        impl_measure_fields(quote::quote!(#name::#variant_name), &variant.fields);
                    Ok(quote::quote! {
                        #extract_fields => {
                            <#tag_type as ::byte::Measure<__Ctx>>::measure(&#tag, ctx) + #body
                        }
                    })
                })
                .collect::<syn::Result<Vec<_>>>()?;

            quote::quote! {
                match self {
                    #(#variants)*
                }
            }
        }
        _ => {
            return Err(syn::Error::new(
                input.span(),
                "TryWrite can only be derived for structs",
            ))
        }
    };

    let predicates = where_clause.map(|w| &w.predicates);
    let constraints = generic_constraints(&input.generics, quote::quote!(::byte::Measure<__Ctx>));
    Ok(quote::quote! {
        impl #impl_generics ::byte::Measure<__Ctx> for #name #ty_generics
            where
                __Ctx: ::std::marker::Copy,
                #( #constraints, )*
                #predicates {

            #[allow(unused_variables)]
            fn measure(&self, ctx: __Ctx) -> usize {
                #body
            }
        }
    })
}

fn impl_measure_fields(
    name: proc_macro2::TokenStream,
    fields: &syn::Fields,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let it = fields.into_iter();
    let field_names = it.clone().enumerate().map(|(i, field)| {
        field
            .ident
            .clone()
            .unwrap_or_else(|| syn::Ident::new(&format!("_{i}"), field.span()))
    });

    let field_sizes = it.zip(field_names.clone()).map(|(field, name)| {
        let attrs = match FieldAttributes::parse(&field.attrs) {
            Ok(attrs) => attrs,
            Err(err) => return err.to_compile_error(),
        };
        let ctx = attrs
            .ctx
            .map_or_else(|| quote::quote!(ctx), |ctx| quote::quote!(#ctx));

        if attrs.skip {
            quote::quote!(0)
        } else if let Some(skip_if) = attrs.skip_if {
            quote::quote! {
                if !#skip_if {
                    ::byte::Measure::measure(#name, #ctx)
                } else {
                    0
                }
            }
        } else {
            quote::quote!(::byte::Measure::measure(#name, #ctx))
        }
    });

    let extract_fields = match fields {
        syn::Fields::Named(_) => quote::quote!(#name { #(#field_names),* }),
        syn::Fields::Unnamed(_) => quote::quote!(#name ( #(#field_names),* )),
        syn::Fields::Unit => quote::quote!(#name),
    };
    let body = quote::quote!(0 #(+ #field_sizes)*);

    (extract_fields, body)
}

#[proc_macro_derive(TryRead, attributes(byte))]
pub fn derive_try_read(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    match impl_data_read(ast) {
        Ok(gen) => gen.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro_derive(TryWrite, attributes(byte))]
pub fn derive_try_write(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    match impl_data_write(ast) {
        Ok(gen) => gen.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro_derive(Measure, attributes(byte))]
pub fn derive_measure(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    match impl_data_measure(ast) {
        Ok(gen) => gen.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn generics_with_ctx(generics: &syn::Generics) -> syn::Generics {
    let mut generics = generics.clone();
    generics
        .params
        .push(syn::GenericParam::Type(syn::TypeParam::from(
            syn::Ident::new("__Ctx", generics.span()),
        )));
    generics
}

fn generic_constraints(
    generics: &syn::Generics,
    constraint: proc_macro2::TokenStream,
) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    generics.type_params().map(move |p| {
        let ident = &p.ident;
        quote::quote!(#ident: #constraint)
    })
}

#[derive(Default)]
struct FieldAttributes {
    ctx: Option<syn::Expr>,
    skip_if: Option<syn::Expr>,
    skip: bool,
}

impl FieldAttributes {
    fn parse(attrs: &[syn::Attribute]) -> Result<FieldAttributes, syn::Error> {
        let mut attributes = FieldAttributes {
            ctx: None,
            skip_if: None,
            skip: false,
        };

        for arg in parse_attributes(attrs) {
            let arg = arg?;
            match arg {
                syn::Meta::NameValue(kv) if kv.path.is_ident("ctx") => {
                    attributes.ctx = Some(kv.value);
                }
                syn::Meta::NameValue(kv) if kv.path.is_ident("skip_if") => {
                    attributes.skip_if = Some(kv.value);
                }
                syn::Meta::Path(path) if path.is_ident("skip") => {
                    attributes.skip = true;
                }
                _ => return Err(syn::Error::new(arg.span(), "unknown attribute")),
            }
        }

        Ok(attributes)
    }
}

#[derive(Default)]
struct ContainerAttributes {
    tag_type: Option<syn::Path>,
}

impl ContainerAttributes {
    fn parse(attrs: &[syn::Attribute]) -> Result<ContainerAttributes, syn::Error> {
        let mut attributes = ContainerAttributes { tag_type: None };

        for arg in parse_attributes(attrs) {
            let arg = arg?;
            match arg {
                syn::Meta::NameValue(kv) if kv.path.is_ident("tag_type") => {
                    if let syn::Expr::Path(path) = &kv.value {
                        attributes.tag_type = Some(path.path.clone());
                    } else {
                        return Err(syn::Error::new(kv.value.span(), "tag_type must be a type"));
                    }
                }
                _ => return Err(syn::Error::new(arg.span(), "unknown attribute")),
            }
        }

        Ok(attributes)
    }
}

#[derive(Default)]
struct VariantAttributes {
    tag: Option<syn::Expr>,
}

impl VariantAttributes {
    fn parse(attrs: &[syn::Attribute]) -> Result<VariantAttributes, syn::Error> {
        let mut attributes = VariantAttributes { tag: None };

        for arg in parse_attributes(attrs) {
            let arg = arg?;
            match arg {
                syn::Meta::NameValue(kv) if kv.path.is_ident("tag") => {
                    attributes.tag = Some(kv.value);
                }
                _ => return Err(syn::Error::new(arg.span(), "unknown attribute")),
            }
        }

        Ok(attributes)
    }
}

fn parse_attributes(attrs: &[syn::Attribute]) -> impl Iterator<Item = syn::Result<syn::Meta>> + '_ {
    attrs
        .iter()
        .filter(|attr| attr.path().is_ident("byte"))
        .flat_map(move |arg| {
            let parser = Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated;
            match arg
                .meta
                .require_list()
                .and_then(|list| list.parse_args_with(parser))
            {
                Ok(args) => Box::new(args.into_iter().map(Ok)) as Box<dyn Iterator<Item = _>>,
                Err(err) => Box::new([Err(err)].into_iter()),
            }
        })
}
