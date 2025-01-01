use crate::{check_len, Measure, Result, TryRead, TryWrite};

impl<'a, Ctx> TryRead<'a, Ctx> for bool {
    #[inline]
    fn try_read(bytes: &'a [u8], _ctx: Ctx) -> Result<(Self, usize)> {
        check_len(bytes, 1)?;

        Ok((bytes[0] != 0, 1))
    }
}

impl<Ctx> TryWrite<Ctx> for bool {
    #[inline]
    fn try_write(&self, bytes: &mut [u8], _ctx: Ctx) -> Result<usize> {
        check_len(bytes, 1)?;

        bytes[0] = if *self { u8::MAX } else { 0 };

        Ok(1)
    }
}

impl<Ctx> Measure<Ctx> for bool {
    #[inline]
    fn measure(&self, _: Ctx) -> usize {
        1
    }
}
