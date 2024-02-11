use syn::{punctuated::Punctuated, spanned::Spanned};

#[derive(Default)]
struct FieldAttributes {
    ctx: Option<syn::Expr>,
    skip_if: Option<syn::Expr>,
}

fn impl_struct_read(
    name: &syn::Ident,
    struct_fields: &syn::Fields,
    generics: &syn::Generics,
) -> proc_macro2::TokenStream {
    let mut lifetimes = generics.lifetimes();
    let (_, ty_generics, where_clause) = generics.split_for_impl();

    let mut generics = generics_with_ctx(generics);
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

    let fields = match struct_fields {
        syn::Fields::Named(fields) => &fields.named,
        syn::Fields::Unnamed(fields) => &fields.unnamed,
        syn::Fields::Unit => {
            // no fields, so no bytes to read
            return quote::quote! {
                impl #impl_generics ::byte::TryRead<#input_lt, __Ctx> for #name #ty_generics #where_clause {
                    #[inline]
                    fn try_read(bytes: & #input_lt [u8], ctx: __Ctx) -> ::byte::Result<(Self, usize)> {
                        Ok((Self, 0))
                    }
                }
            };
        }
    };

    if lifetimes.next().is_some() {
        return syn::Error::new(
            generics.span(),
            "only one lifetime parameter is allowed for TryRead derive",
        )
        .to_compile_error();
    }

    let field_names = fields.iter().enumerate().map(|(i, field)| {
        let borrowed_name = field
            .ident
            .clone()
            .unwrap_or_else(|| syn::Ident::new(&format!("_{i}"), field.span()));
        let owned_name = quote::format_ident!("__owned_{i}");
        (owned_name, borrowed_name)
    });

    let field_reads =
        fields
            .iter()
            .zip(field_names.clone())
            .map(|(field, (owned_name, borrowed_name))| {
                let attr = field.attrs.iter().find(|attr| attr.path().is_ident("byte"));

                let attrs = match attr.map_or_else(|| Ok(Default::default()), parse_field_attrs) {
                    Ok(attrs) => attrs,
                    Err(err) => return err.to_compile_error(),
                };

                let ctx = attrs
                    .ctx
                    .map_or_else(|| quote::quote!(ctx), |ctx| quote::quote!(#ctx));
                let ty = &field.ty;
                let read = match attrs.skip_if {
                    Some(skip_if) => {
                        quote::quote! {
                            if #skip_if {
                                <#ty>::default()
                            } else {
                                ::byte::BytesExt::read(bytes, __offset, #ctx)?
                            }
                        }
                    }
                    None => {
                        quote::quote!(::byte::BytesExt::read(bytes, __offset, #ctx)?)
                    }
                };
                quote::quote! {
                    let #owned_name: #ty = #read;
                    #[allow(unused_variables)]
                    let #borrowed_name: &#ty = &#owned_name;
                }
            });

    let result = match struct_fields {
        syn::Fields::Named(_) => {
            let it = field_names
                .map(|(owned_name, borrowed_name)| quote::quote!(#borrowed_name: #owned_name));
            quote::quote!(Self { #(#it),* })
        }
        syn::Fields::Unnamed(_) => {
            let it = field_names.map(|(owned_name, _)| owned_name);
            quote::quote!(Self(#(#it),*))
        }
        syn::Fields::Unit => unreachable!(),
    };
    let predicates = where_clause.map(|w| &w.predicates);

    quote::quote! {
        impl #impl_generics ::byte::TryRead<#input_lt, __Ctx> for #name #ty_generics
            where
                __Ctx: ::byte::ctx::Endianess,
                #predicates {
            fn try_read(bytes: & #input_lt [u8], ctx: __Ctx) -> ::byte::Result<(Self, usize)> {
                let __offset = &mut 0;
                #(#field_reads)*
                Ok((#result, *__offset))
            }
        }
    }
}

fn impl_struct_write(
    name: &syn::Ident,
    struct_fields: &syn::Fields,
    generics: &syn::Generics,
) -> proc_macro2::TokenStream {
    let (_, ty_generics, where_clause) = generics.split_for_impl();

    let generics = generics_with_ctx(generics);
    let (impl_generics, _, _) = generics.split_for_impl();

    let fields = match struct_fields {
        syn::Fields::Named(fields) => &fields.named,
        syn::Fields::Unnamed(fields) => &fields.unnamed,
        syn::Fields::Unit => {
            // no fields, so no bytes to read
            return quote::quote! {
                impl #impl_generics ::byte::TryWrite<__Ctx> for #name #ty_generics #where_clause {
                    #[inline]
                    fn try_write(&self, bytes: &mut [u8], ctx: __Ctx) -> ::byte::Result<usize> {
                        Ok(0)
                    }
                }
            };
        }
    };

    let field_names = fields.iter().enumerate().map(|(i, field)| {
        field
            .ident
            .clone()
            .unwrap_or_else(|| syn::Ident::new(&format!("_{i}"), field.span()))
    });

    let field_writes = fields.iter().zip(field_names.clone()).map(|(field, name)| {
        let attr = field.attrs.iter().find(|attr| attr.path().is_ident("byte"));
        let attrs = match attr.map_or_else(|| Ok(Default::default()), parse_field_attrs) {
            Ok(attrs) => attrs,
            Err(err) => return err.to_compile_error(),
        };
        let ctx = attrs
            .ctx
            .map_or_else(|| quote::quote!(ctx), |ctx| quote::quote!(#ctx));
        match attrs.skip_if {
            Some(skip_if) => {
                quote::quote! {
                    if !#skip_if {
                        ::byte::BytesExt::write(bytes, __offset, #name, #ctx)?;
                    }
                }
            }
            None => quote::quote!(::byte::BytesExt::write(bytes, __offset, #name, #ctx)?;),
        }
    });

    let extract_fields = match struct_fields {
        syn::Fields::Named(_) => Some(quote::quote! {
            let #name { #(#field_names),* } = self;
        }),
        syn::Fields::Unnamed(_) => Some(quote::quote! {
            let #name ( #(#field_names),* ) = self;
        }),
        syn::Fields::Unit => unreachable!(),
    };
    let predicates = where_clause.map(|w| &w.predicates);

    quote::quote! {
        impl #impl_generics ::byte::TryWrite<__Ctx> for #name #ty_generics
            where
                __Ctx: ::byte::ctx::Endianess,
                #predicates {
            fn try_write(&self, bytes: &mut [u8], ctx: __Ctx) -> ::byte::Result<usize> {
                let __offset = &mut 0;
                #extract_fields
                #(#field_writes)*
                Ok(*__offset)
            }
        }
    }
}

fn impl_struct_measure(
    name: &syn::Ident,
    struct_fields: &syn::Fields,
    generics: &syn::Generics,
) -> proc_macro2::TokenStream {
    let (_, ty_generics, where_clause) = generics.split_for_impl();

    let generics = generics_with_ctx(generics);
    let (impl_generics, _, _) = generics.split_for_impl();

    let fields = match struct_fields {
        syn::Fields::Named(fields) => &fields.named,
        syn::Fields::Unnamed(fields) => &fields.unnamed,
        syn::Fields::Unit => {
            // no fields, so no bytes to read
            return quote::quote! {
                impl #impl_generics ::byte::Measure<__Ctx> for #name #ty_generics #where_clause {
                    #[inline]
                    fn measure(&self, _: Ctx) -> usize {
                        0
                    }
                }
            };
        }
    };

    let field_names = fields.iter().enumerate().map(|(i, field)| {
        field
            .ident
            .clone()
            .unwrap_or_else(|| syn::Ident::new(&format!("_{i}"), field.span()))
    });

    let field_sizes = fields.iter().zip(field_names.clone()).map(|(field, name)| {
        let attr = field.attrs.iter().find(|attr| attr.path().is_ident("byte"));
        let attrs = match attr.map_or_else(|| Ok(Default::default()), parse_field_attrs) {
            Ok(attrs) => attrs,
            Err(err) => return err.to_compile_error(),
        };
        let ctx = attrs
            .ctx
            .map_or_else(|| quote::quote!(ctx), |ctx| quote::quote!(#ctx));
        match attrs.skip_if {
            Some(skip_if) => {
                quote::quote! {
                    if !#skip_if {
                        ::byte::Measure::measure(#name, #ctx)
                    } else {
                        0
                    }
                }
            }
            None => quote::quote!(::byte::Measure::measure(#name, #ctx)),
        }
    });

    let extract_fields = match struct_fields {
        syn::Fields::Named(_) => Some(quote::quote! {
            let #name { #(#field_names),* } = self;
        }),
        syn::Fields::Unnamed(_) => Some(quote::quote! {
            let #name ( #(#field_names),* ) = self;
        }),
        syn::Fields::Unit => unreachable!(),
    };
    let predicates = where_clause.map(|w| &w.predicates);

    quote::quote! {
        impl #impl_generics ::byte::Measure<__Ctx> for #name #ty_generics
            where
                __Ctx: ::byte::ctx::Endianess,
                #predicates {
            fn measure(&self, ctx: __Ctx) -> usize {
                #extract_fields
                0 #(+ #field_sizes)*
            }
        }
    }
}

fn impl_try_read(ast: &syn::DeriveInput) -> proc_macro2::TokenStream {
    match &ast.data {
        syn::Data::Struct(data) => impl_struct_read(&ast.ident, &data.fields, &ast.generics),
        _ => syn::Error::new(ast.span(), "TryRead can only be derived for structs")
            .to_compile_error(),
    }
}

fn impl_try_write(ast: &syn::DeriveInput) -> proc_macro2::TokenStream {
    match &ast.data {
        syn::Data::Struct(data) => impl_struct_write(&ast.ident, &data.fields, &ast.generics),
        _ => syn::Error::new(ast.span(), "TryWrite can only be derived for structs")
            .to_compile_error(),
    }
}

fn impl_measure(ast: &syn::DeriveInput) -> proc_macro2::TokenStream {
    match &ast.data {
        syn::Data::Struct(data) => impl_struct_measure(&ast.ident, &data.fields, &ast.generics),
        _ => syn::Error::new(ast.span(), "Measure can only be derived for structs")
            .to_compile_error(),
    }
}

#[proc_macro_derive(TryRead, attributes(byte))]
pub fn derive_try_read(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let gen = impl_try_read(&ast);
    gen.into()
}

#[proc_macro_derive(TryWrite, attributes(byte))]
pub fn derive_try_write(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let gen = impl_try_write(&ast);
    gen.into()
}

#[proc_macro_derive(Measure, attributes(byte))]
pub fn derive_measure(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let gen = impl_measure(&ast);
    gen.into()
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

fn parse_field_attrs(attr: &syn::Attribute) -> Result<FieldAttributes, syn::Error> {
    let parser = Punctuated::<syn::MetaNameValue, syn::Token![,]>::parse_terminated;
    let args = attr.meta.require_list()?.parse_args_with(parser)?;

    let mut attributes = FieldAttributes {
        ctx: None,
        skip_if: None,
    };

    for arg in args {
        if arg.path.is_ident("ctx") {
            attributes.ctx = Some(arg.value);
        } else if arg.path.is_ident("skip_if") {
            attributes.skip_if = Some(arg.value);
        } else {
            return Err(syn::Error::new(arg.path.span(), "unknown attribute"));
        }
    }

    Ok(attributes)
}
