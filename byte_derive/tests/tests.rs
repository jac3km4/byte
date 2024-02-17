use byte::{
    ctx::{Delimiter, Len},
    Measure, TryRead, TryWrite, LE,
};

#[derive(Debug, Clone, PartialEq, TryWrite, TryRead, Measure)]
#[byte(tag_type = u8)]
enum Enum {
    #[byte(tag = 0x1)]
    A(u32, f64),
    #[byte(tag = 0x2)]
    B(u32),
    #[byte(tag = 0x3)]
    C,
}

#[test]
fn test_enum() {
    let data = Enum::A(0x12345678, 1234.5678);
    let buf = &mut [0; 13];
    data.try_write(buf, LE).unwrap();
    assert_eq!(data.measure(LE), 13);
    assert_eq!(Ok((data, 13)), Enum::try_read(buf, LE));

    let data = Enum::B(0x12345678);
    let buf = &mut [0; 5];
    data.try_write(buf, LE).unwrap();
    assert_eq!(data.measure(LE), 5);
    assert_eq!(Ok((data, 5)), Enum::try_read(buf, LE));

    let data = Enum::C;
    let buf = &mut [0; 1];
    data.try_write(buf, LE).unwrap();
    assert_eq!(data.measure(LE), 1);
    assert_eq!(Ok((data, 1)), Enum::try_read(buf, LE));
}

#[derive(Debug, Clone, PartialEq, TryWrite, TryRead, Measure)]
struct Named<'a> {
    id: u32,
    timestamp: f64,
    #[byte(ctx = Delimiter(0))]
    str: &'a str,
}

#[test]
fn test_named_struct() {
    let data: Named = Named {
        id: 0x12345678,
        timestamp: 1234.5678,
        str: "hello",
    };
    let buf = &mut [0; 18];
    data.try_write(buf, LE).unwrap();
    assert_eq!(data.measure(LE), 18);
    assert_eq!(Ok((data, 18)), Named::try_read(buf, LE));
}

#[derive(Debug, Clone, PartialEq, TryWrite, TryRead, Measure)]
struct TypeArg<A> {
    id: A,
    timestamp: f64,
}

#[test]
fn test_no_lifetime_struct() {
    let data = TypeArg {
        id: 0x12345678,
        timestamp: 1234.5678,
    };
    let buf = &mut [0; 12];
    data.try_write(buf, LE).unwrap();
    assert_eq!(data.measure(LE), 12);
    assert_eq!(Ok((data, 12)), TypeArg::try_read(buf, LE));
}

#[derive(Debug, Clone, PartialEq, TryWrite, TryRead)]
struct FieldDependent<'a> {
    len: usize,
    #[byte(ctx = Len(*len))]
    str: &'a str,
}

#[test]
fn test_len_dependent() {
    let data = FieldDependent {
        len: 2,
        str: "hello",
    };
    let buf = &mut [0; 13];
    data.try_write(buf, LE).unwrap();
    assert_eq!(
        Ok((FieldDependent { len: 2, str: "he" }, 10)),
        FieldDependent::try_read(buf, LE)
    );
}

#[derive(Debug, Clone, PartialEq, TryWrite, TryRead)]
struct Skipped {
    bool: bool,
    #[byte(skip_if = *bool)]
    other: u32,
    #[byte(skip)]
    unused: u32,
}

#[test]
fn test_skipped() {
    let data = Skipped {
        bool: true,
        other: 0x12345678,
        unused: 0x12345678,
    };
    let buf = &mut [0; 5];
    data.try_write(buf, LE).unwrap();
    assert_eq!(
        Ok((
            Skipped {
                bool: true,
                other: 0,
                unused: 0
            },
            1
        )),
        Skipped::try_read(buf, LE)
    );
}

#[test]
fn test_not_skipped() {
    let data = Skipped {
        bool: false,
        other: 0x12345678,
        unused: 0x12345678,
    };
    let buf = &mut [0; 5];
    data.try_write(buf, LE).unwrap();
    assert_eq!(
        Ok((
            Skipped {
                bool: false,
                other: 0x12345678,
                unused: 0
            },
            5
        )),
        Skipped::try_read(buf, LE)
    );
}

#[derive(Debug, Clone, PartialEq, TryRead, TryWrite)]
struct Tuple<'a>(u32, f64, #[byte(ctx = Delimiter(0))] &'a str);

#[test]
fn test_tuple_struct() {
    let data: Tuple = Tuple(0x12345678, 1234.5678, "hello");
    let buf = &mut [0; 18];
    data.try_write(buf, LE).unwrap();
    assert_eq!(Ok((data, 18)), Tuple::try_read(buf, LE));
}

#[derive(Debug, Clone, PartialEq, TryRead, TryWrite)]
struct Empty;

#[test]
fn test_empty_struct() {
    let data: Empty = Empty;
    let buf = &mut [0; 0];
    data.try_write(buf, LE).unwrap();
    assert_eq!(Ok((data, 0)), Empty::try_read(buf, LE));
}
