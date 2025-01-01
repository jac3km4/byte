//! A low-level, zero-copy and panic-free binary serializer and deserializer.
//!
//! # Usage
//!
//! First, add the following to your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! byte = "0.3"
//! ```
//!
//! `Byte` is a `no_std` library; it can be used in any `#![no_std]` situation or crate.
//!
//! # Overview
//!
//! `Byte` is designed for encoding or decoding binary data in a fast and low level way.
//! A classical use case is I2C communication packages encoding.
//!
//! `Byte` provides two core traits `TryRead` and `TryWrite`.
//! Types implement these traits can be serialized into or deserialized from byte slices.
//!
//! The library is meant to be simple, and it will always be.
//!
//! # Examples
//!
//! Deserialize a `u32` from bytes:
//!
//! ```
//! use byte::*;
//!
//! let bytes: &[u8] = &[0xde, 0xad, 0xbe, 0xef];
//!
//! let offset = &mut 0;
//! let num = bytes.read::<u32>(offset, BE).unwrap();
//! assert_eq!(num, 0xdeadbeef);
//! assert_eq!(*offset, 4);
//! ```
//!
//! Deserialize a `&str` from bytes:
//!
//! ```
//! use byte::*;
//! use byte::ctx::{Delimiter, NULL};
//!
//! let bytes: &[u8] = b"hello, world!\0dump";
//!
//! let offset = &mut 0;
//! let str = bytes.read::<&str>(offset, Delimiter(NULL)).unwrap();
//! assert_eq!(str, "hello, world!");
//! assert_eq!(*offset, 14);
//! ```
//!
//! `Byte` supports serializing and deserializing language primitives by default.
//!
//! - `&str` (with `Str` context)
//! - `&[u8]` (with `Byte` context)
//! - `u8`, `i8`, `u64`, `f64` ... (with `Endian` context)
//! - `bool`
//!
//! # Define custom serializable/deserializable types
//!
//! In this example, we implement `TryRead` and `TryWrite` for the `Header` type,
//! which has a variable-length name and a boolean field.
//!
//! ## Binary Structure
//!
//! ```text
//! |       | Name's Length (Big Endian) |                Name              | Enabled |
//! | ----- | -------------------------- | ---- | ---- | ---- | ---- | ---- | ------- |
//! | Byte  | 0            | 5           | 'H'  | 'E'  | 'L'  | 'L'  | 'O'  | 0       |
//! ```
//!
//! ## Example
//!
//! The only thing you may be curious about is the returned usize;
//! that's the number of bytes consumed by the read/write operation.
//!
//! ```
//! use byte::*;
//! use byte::ctx::*;
//!
//! struct Header<'a> {
//!     name: &'a str,
//!     enabled: bool,
//! }
//!
//! impl<'a, Ctx: Endianess> TryRead<'a, Ctx> for Header<'a> {
//!     fn try_read(bytes: &'a [u8], endian: Ctx) -> Result<(Self, usize)> {
//!         let offset = &mut 0;
//!
//!         let name_len = bytes.read::<u16>(offset, endian)? as usize;
//!         let header = Header {
//!             name: bytes.read::<&str>(offset, Len(name_len))?,
//!             enabled: bytes.read::<bool>(offset, ())?,
//!         };
//!
//!         Ok((header, *offset))
//!     }
//! }
//!
//! impl<'a, Ctx: Endianess> TryWrite<Ctx> for Header<'a> {
//!     fn try_write(&self, bytes: &mut [u8], endian: Ctx) -> Result<usize> {
//!         let offset = &mut 0;
//!
//!         bytes.write::<u16>(offset, &(self.name.len() as u16), endian)?;
//!         bytes.write::<str>(offset, self.name, ())?;
//!         bytes.write::<bool>(offset, &self.enabled, ())?;
//!
//!         Ok(*offset)
//!     }
//! }
//! ```
//!
//! ## Usage
//!
//! ```ignore
//! let bytes = [0, 5, b"H"[0], b"E"[0], b"L"[0], b"L"[0], b"O"[0], 0];
//!
//! let header: Header = bytes.read(&mut 0, BE).unwrap();
//!
//! assert_eq!(header.name, "HELLO");
//! assert_eq!(header.enabled, false);
//!
//! let mut write = [0u8; 8];
//! write.write(&mut 0, header, BE).unwrap();
//! assert_eq!(write, bytes);
//! ```

#![no_std]
#![forbid(unsafe_code)]

#[cfg(feature = "alloc")]
extern crate alloc;

pub mod ctx;
#[cfg(feature = "alloc")]
use alloc::borrow::Cow;
#[cfg(feature = "alloc")]
use alloc::vec::Vec;
use core::{error, fmt, marker::PhantomData};
pub use ctx::{BE, LE};

#[cfg(feature = "derive")]
pub use byte_derive::{Measure, TryRead, TryWrite};

/// A specialized Result type for `Byte`
pub type Result<T> = core::result::Result<T, Error>;

/// The error type for the `byte` crate.
///
/// - `Error::BadOffset` will be returned when the offset parameter exceeds the slice's length.
///
/// - `Error::BadInput` and `Error::Incomplete` will be returned when `try_read()` or
///   `try_write()` finds the bytes are invalid or not long enough to determine their validity.
///
/// Note that we usually use `bytes.read()` in `try_read()` which may return `Error::BadOffset`,
/// indicating incomplete data. So the error will automatically be converted into
/// `Error::Incomplete` if you use `bytes.read()` (the same applies to `write()`).
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Error {
    /// The requested data is bigger than the available range
    Incomplete,
    /// The offset is invalid
    BadOffset(usize),
    /// The requested data content is invalid
    BadInput { err: &'static str },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Incomplete => write!(f, "incomplete data"),
            Error::BadOffset(offset) => write!(f, "bad offset: {}", offset),
            Error::BadInput { err } => write!(f, "bad input: {}", err),
        }
    }
}

impl error::Error for Error {}

/// A helper function that checks whether the given length exceeded the length
/// of the slice; returns `Err(Error::Incomplete)` otherwise.
///
/// # Example
///
/// ```
/// use byte::*;
///
/// let bytes = [0u8; 4];
/// assert_eq!(check_len(&bytes, 4), Ok(4));
/// assert_eq!(check_len(&bytes, 5), Err(Error::Incomplete));
/// ```
#[inline]
pub fn check_len(bytes: &[u8], len: usize) -> Result<usize> {
    if bytes.len() < len {
        Err(Error::Incomplete)
    } else {
        Ok(len)
    }
}

/// A data structure that can be deserialized.
/// Types implementing this trait can be `read()` from a byte slice.
pub trait TryRead<'a, Ctx = ()>
where
    Self: Sized,
{
    /// Try to read from a byte slice using a specific context.
    ///
    /// Read the value out of bytes; the bytes passed in are splitted by offset
    /// and should be read at head.
    /// If successful, `try_read()` should return a tuple with the value and the
    /// number of bytes consumed.
    ///
    /// # Example
    ///
    /// ```
    /// use byte::*;
    ///
    /// // Demo type showing how to read boolean from bytes.
    /// // This functionality is already provided by this crate.
    /// pub struct Bool(bool);
    ///
    /// impl<'a> TryRead<'a> for Bool {
    ///     #[inline]
    ///     fn try_read(bytes: &'a [u8], _ctx: ()) -> Result<(Self, usize)> {
    ///         check_len(bytes, 1)?;
    ///
    ///         Ok((Bool(bytes[0] != 0), 1))
    ///     }
    /// }
    /// ```
    fn try_read(bytes: &'a [u8], ctx: Ctx) -> Result<(Self, usize)>;
}

#[cfg(feature = "alloc")]
impl<'a, A, Ctx> TryRead<'a, Ctx> for Cow<'a, A>
where
    A: Clone + ?Sized,
    &'a A: TryRead<'a, Ctx>,
{
    #[inline]
    fn try_read(bytes: &'a [u8], ctx: Ctx) -> Result<(Self, usize)> {
        let (borrowed, size) = <&'a A>::try_read(bytes, ctx)?;
        Ok((Cow::Borrowed(borrowed), size))
    }
}

#[cfg(feature = "alloc")]
impl<'a, A, Ctx> TryRead<'a, Ctx> for alloc::boxed::Box<A>
where
    A: TryRead<'a, Ctx> + ?Sized,
{
    #[inline]
    fn try_read(bytes: &'a [u8], ctx: Ctx) -> Result<(Self, usize)> {
        let (value, size) = A::try_read(bytes, ctx)?;
        Ok((alloc::boxed::Box::new(value), size))
    }
}

impl<'a, A, Ctx, const N: usize> TryRead<'a, Ctx> for [A; N]
where
    A: TryRead<'a, Ctx> + Default,
    Ctx: Copy,
{
    fn try_read(bytes: &'a [u8], ctx: Ctx) -> Result<(Self, usize)> {
        let offset = &mut 0;
        let mut arr = core::array::from_fn(|_| A::default());
        for el in &mut arr {
            *el = bytes.read(offset, ctx)?
        }
        Ok((arr, *offset))
    }
}

/// A data structure that can be serialized.
/// Types implement this trait can be `write()` into a byte slice.
pub trait TryWrite<Ctx = ()> {
    /// Try to write to a byte slice using a specific context.
    ///
    /// Write the value into bytes; the bytes passed in are splitted by offset
    /// and should be written at head.
    /// If successful `try_write()` should return the number of bytes written.
    ///
    /// # Example
    ///
    /// ```
    /// use byte::*;
    ///
    /// pub struct HasBool(bool);
    ///
    /// impl TryWrite for HasBool {
    ///     #[inline]
    ///     fn try_write(&self, bytes: &mut [u8], _ctx: ()) -> Result<usize> {
    ///         check_len(bytes, 1)?;
    ///
    ///         bytes[0] = if self.0 { u8::max_value() } else { 0 };
    ///
    ///         Ok(1)
    ///     }
    /// }
    /// ```
    fn try_write(&self, bytes: &mut [u8], ctx: Ctx) -> Result<usize>;
}

impl<A, Ctx> TryWrite<Ctx> for &A
where
    A: TryWrite<Ctx> + ?Sized,
{
    #[inline]
    fn try_write(&self, bytes: &mut [u8], ctx: Ctx) -> Result<usize> {
        (*self).try_write(bytes, ctx)
    }
}

#[cfg(feature = "alloc")]
impl<A, Ctx> TryWrite<Ctx> for Cow<'_, A>
where
    A: Clone + TryWrite<Ctx> + ?Sized,
{
    #[inline]
    fn try_write(&self, bytes: &mut [u8], ctx: Ctx) -> Result<usize> {
        self.as_ref().try_write(bytes, ctx)
    }
}

#[cfg(feature = "alloc")]
impl<A, Ctx> TryWrite<Ctx> for alloc::boxed::Box<A>
where
    A: TryWrite<Ctx> + ?Sized,
{
    #[inline]
    fn try_write(&self, bytes: &mut [u8], ctx: Ctx) -> Result<usize> {
        self.as_ref().try_write(bytes, ctx)
    }
}

impl<A, Ctx, const N: usize> TryWrite<Ctx> for [A; N]
where
    A: TryWrite<Ctx>,
    Ctx: Copy,
{
    fn try_write(&self, bytes: &mut [u8], ctx: Ctx) -> Result<usize> {
        let offset = &mut 0;
        for val in self {
            bytes.write(offset, val, ctx)?;
        }
        Ok(*offset)
    }
}

/// A data structure with a measurable size.
pub trait Measure<Ctx = ()> {
    /// Measure how many bytes will be written to a byte slice using a specific context.
    ///
    /// # Example
    ///
    /// ```
    /// use byte::*;
    ///
    /// pub struct HasBool(bool);
    ///
    /// impl Measure for HasBool {
    ///     #[inline]
    ///     fn measure(&self, _ctx: ()) -> usize {
    ///         1
    ///     }
    /// }
    /// ```
    fn measure(&self, ctx: Ctx) -> usize;
}

impl<A, Ctx> Measure<Ctx> for &A
where
    A: Measure<Ctx> + ?Sized,
{
    #[inline]
    fn measure(&self, ctx: Ctx) -> usize {
        (*self).measure(ctx)
    }
}

#[cfg(feature = "alloc")]
impl<A, Ctx> Measure<Ctx> for Cow<'_, A>
where
    A: Clone + Measure<Ctx> + ?Sized,
{
    #[inline]
    fn measure(&self, ctx: Ctx) -> usize {
        self.as_ref().measure(ctx)
    }
}

#[cfg(feature = "alloc")]
impl<A, Ctx> Measure<Ctx> for alloc::boxed::Box<A>
where
    A: Measure<Ctx> + ?Sized,
{
    #[inline]
    fn measure(&self, ctx: Ctx) -> usize {
        self.as_ref().measure(ctx)
    }
}

impl<A, Ctx, const N: usize> Measure<Ctx> for [A; N]
where
    A: Measure<Ctx>,
    Ctx: Copy,
{
    fn measure(&self, ctx: Ctx) -> usize {
        let mut size = 0;
        for val in self {
            size += val.measure(ctx);
        }
        size
    }
}

/// Extension methods for byte slices.
///
/// # Offset
///
/// The offset is the first parameter of each method.
///
/// It tells the starting position, and will be increased by the number
/// which will be increased by size the operation consumed.
pub trait BytesExt<Ctx> {
    /// Reads a value from a byte slice specifying the context.
    ///
    /// # Example
    ///
    /// ```
    /// use byte::*;
    /// use byte::ctx::*;
    ///
    /// let bytes: &[u8] = b"hello, world!";
    ///
    /// let str: &str = bytes.read(&mut 0, Delimiter(b"!"[0])).unwrap();
    /// assert_eq!(str, "hello, world");
    /// ```
    fn read<'a, T>(&'a self, offset: &mut usize, ctx: Ctx) -> Result<T>
    where
        T: TryRead<'a, Ctx>;

    #[inline]
    fn read_at<'a, T>(&'a self, mut offset: usize, ctx: Ctx) -> Result<T>
    where
        T: TryRead<'a, Ctx>,
    {
        self.read(&mut offset, ctx)
    }

    /// Reads multiple values of the same type using an iterator.
    ///
    /// # Example
    ///
    /// ```
    /// use byte::*;
    /// use byte::ctx::*;
    ///
    /// let bytes: &[u8] = b"hello\0world\0dead\0beef\0more";
    /// let mut offset = 0;
    /// {
    ///     let mut iter = bytes.read_iter(&mut offset, Delimiter(NULL)).take(4);
    ///     assert_eq!(iter.next(), Some(Ok("hello")));
    ///     assert_eq!(iter.next(), Some(Ok("world")));
    ///     assert_eq!(iter.next(), Some(Ok("dead")));
    ///     assert_eq!(iter.next(), Some(Ok("beef")));
    ///     assert_eq!(iter.next(), None);
    /// }
    /// assert_eq!(offset, 22);
    /// ```
    fn read_iter<'a, 'i, T>(&'a self, offset: &'i mut usize, ctx: Ctx) -> Iter<'a, 'i, T, Ctx>
    where
        T: TryRead<'a, Ctx>,
        Ctx: Clone;

    /// Writes a value into a byte slice specifiying the context.
    ///
    /// # Example
    ///
    /// ```
    /// use byte::*;
    /// use byte::ctx::*;
    ///
    /// let mut bytes_be = [0u8; 2];
    /// let mut bytes_le = [0u8; 2];
    ///
    /// bytes_be.write::<u16>(&mut 0, &0xff, BE).unwrap();
    /// bytes_le.write::<u16>(&mut 0, &0xff, LE).unwrap();
    ///
    /// assert_eq!(bytes_be, [0, 0xff]);
    /// assert_eq!(bytes_le, [0xff, 0]);
    /// ```
    fn write<T>(&mut self, offset: &mut usize, t: &T, ctx: Ctx) -> Result<()>
    where
        T: TryWrite<Ctx> + ?Sized;

    #[inline]
    fn write_at<T>(&mut self, mut offset: usize, t: &T, ctx: Ctx) -> Result<()>
    where
        T: TryWrite<Ctx> + ?Sized,
    {
        self.write(&mut offset, t, ctx)
    }
}

impl<Ctx> BytesExt<Ctx> for [u8] {
    #[inline]
    fn read<'a, T>(&'a self, offset: &mut usize, ctx: Ctx) -> Result<T>
    where
        T: TryRead<'a, Ctx>,
    {
        let slice = self;

        if *offset > slice.len() {
            return Err(Error::BadOffset(*offset));
        };

        match TryRead::try_read(&slice[*offset..], ctx) {
            Ok((t, size)) => {
                *offset += size;
                Ok(t)
            }
            Err(Error::BadOffset(_)) => Err(Error::Incomplete),
            Err(err) => Err(err),
        }
    }

    fn read_iter<'a, 'i, T>(&'a self, offset: &'i mut usize, ctx: Ctx) -> Iter<'a, 'i, T, Ctx>
    where
        T: TryRead<'a, Ctx>,
        Ctx: Clone,
    {
        Iter {
            bytes: self,
            offset,
            ctx,
            phantom: PhantomData,
        }
    }

    fn write<T>(&mut self, offset: &mut usize, t: &T, ctx: Ctx) -> Result<()>
    where
        T: TryWrite<Ctx> + ?Sized,
    {
        let slice = self;

        if *offset > slice.len() {
            return Err(Error::BadOffset(*offset));
        };

        match TryWrite::try_write(t, &mut slice[*offset..], ctx) {
            Ok(size) => {
                *offset += size;
                Ok(())
            }
            Err(Error::BadOffset(_)) => Err(Error::Incomplete),
            Err(err) => Err(err),
        }
    }
}

#[cfg(feature = "alloc")]
/// Extension methods for values that can be measured and serialized.
pub trait ToBytesExt<Ctx>: Sized + Measure<Ctx> + TryWrite<Ctx> {
    /// Allocates a `Vec` with size based on the result of `measure()` and writes the value into it.
    /// Expecting the context to be specified.
    ///
    /// # Example
    ///
    /// ```
    /// use byte::*;
    ///
    /// assert_eq!(0xFFu32.to_bytes(BE), Ok(vec![0, 0, 0, 0xff]));
    /// ```
    fn to_bytes(&self, ctx: Ctx) -> Result<Vec<u8>>;
}

#[cfg(feature = "alloc")]
impl<Ctx, A> ToBytesExt<Ctx> for A
where
    Ctx: Copy,
    A: Measure<Ctx> + TryWrite<Ctx>,
{
    fn to_bytes(&self, ctx: Ctx) -> Result<Vec<u8>> {
        let mut bytes = alloc::vec![0; self.measure(ctx)];
        self.try_write(&mut bytes, ctx)?;
        Ok(bytes)
    }
}

/// An iterator that reads values of the same type from a byte slice.
///
/// # Example
///
/// ```
/// use byte::*;
/// use byte::ctx::*;
///
/// let bytes: &[u8] = b"hello\0world\0dead\0beef\0more";
/// let mut offset = 0;
/// {
///     let mut iter = bytes.read_iter(&mut offset, Delimiter(NULL)).take(4);
///     assert_eq!(iter.next(), Some(Ok("hello")));
///     assert_eq!(iter.next(), Some(Ok("world")));
///     assert_eq!(iter.next(), Some(Ok("dead")));
///     assert_eq!(iter.next(), Some(Ok("beef")));
///     assert_eq!(iter.next(), None);
/// }
/// assert_eq!(offset, 22);
/// ```
#[must_use = "iterator adaptors are lazy and do nothing unless consumed"]
pub struct Iter<'a, 'i, T, Ctx>
where
    T: TryRead<'a, Ctx>,
    Ctx: Clone,
{
    bytes: &'a [u8],
    offset: &'i mut usize,
    ctx: Ctx,
    phantom: PhantomData<T>,
}

impl<'a, T, Ctx> Iterator for Iter<'a, '_, T, Ctx>
where
    T: TryRead<'a, Ctx>,
    Ctx: Clone,
{
    type Item = Result<T>;

    #[inline]
    fn next(&mut self) -> Option<Result<T>> {
        match TryRead::try_read(&self.bytes[*self.offset..], self.ctx.clone()) {
            Ok((t, size)) => {
                *self.offset += size;
                Some(Ok(t))
            }
            Err(err) => Some(Err(err)),
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}
