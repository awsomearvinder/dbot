#![allow(unused_variables)]
use serde::{
    de::{self, SeqAccess, Visitor},
    Deserialize,
};
use unicode_segmentation::UnicodeSegmentation;
pub struct Deserializer<'de> {
    // We truncate off part of the string as it's serialized to another form, when there's nothing left we're done.
    // Commands *shouldn't* require look back to deserialize, that is a preceeding token shouldn't impact what the next
    // token is serialized as.
    // note: We truncate starting from the back. The reason being strings will always be starting from the front, and they can be
    // as long as you want, and can include types like integers and the like.
    input: &'de str,
}
impl<'a> From<&'a str> for Deserializer<'a> {
    fn from(s: &'a str) -> Self {
        Self { input: s }
    }
}
//TODO: Implement these
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Error<'a> {
    EndedInQuotes,
    TrailingCharacters { trailing: &'a str },
    InvalidType,
}

impl std::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error")
    }
}
impl std::error::Error for Error<'_> {}
impl serde::de::Error for Error<'_> {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        eprintln!("{}", msg);
        Error::InvalidType
    }
}
pub fn from_str<'a, T>(s: &'a str) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    let mut d = Deserializer::from(s);
    let t = T::deserialize(&mut d);
    if d.input.is_empty() {
        t
    } else {
        Err(Error::TrailingCharacters { trailing: d.input })
    }
}
impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de>
where
    'de: 'a,
{
    type Error = Error<'de>;
    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        use std::ops::ControlFlow;
        self.input = self.input.trim();
        let mut graphemes = self.input.grapheme_indices(true);
        let s = graphemes.try_fold((String::new(), false, 0), |(acc, escaped, _), (i, next)| {
            if escaped {
                return ControlFlow::Continue((acc + next, false, i + 1));
            }
            if next == "\\" {
                return ControlFlow::Continue((acc, true, i + 1));
            }
            if next == "\"" && acc.is_empty() {
                return ControlFlow::Continue((acc, false, i + 1));
            }
            if next == "\"" {
                return ControlFlow::Break((acc, i + 1));
            }
            ControlFlow::Continue((acc + next, false, i + 1))
        });

        let (s, byte_offset) = match s {
            ControlFlow::Continue((_, true, _)) => return Err(Error::EndedInQuotes),
            ControlFlow::Continue((acc, _, byte_offset)) => (acc, byte_offset),
            ControlFlow::Break((acc, byte_offset)) => (acc, byte_offset),
        };
        // Is there a way to get rid of this index?
        // Changing the code above might cause a panic currently if byte_offset >= len (although we know that it won't ever be that.)
        self.input = &self.input[byte_offset..];
        visitor.visit_string(s)
    }

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(CommandList::new(self))
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }
}
struct CommandList<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}
impl<'a, 'de: 'a> CommandList<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self { de }
    }
}
impl<'a, 'de: 'a> SeqAccess<'de> for CommandList<'a, 'de> {
    type Error = Error<'de>;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.de).map(Some)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn string_impl_no_quotes() {
        assert_eq!(
            from_str("test one two three"),
            Ok(String::from("test one two three"))
        )
    }
    #[test]
    fn string_impl_quotes() {
        assert_eq!(
            from_str::<String>("\"test one two three\""),
            Ok(String::from("test one two three"))
        )
    }
    #[test]
    fn string_impl_quotes_trailing() {
        assert_eq!(
            from_str::<String>("\"test one two three\" a"),
            Err(Error::TrailingCharacters { trailing: " a" })
        )
    }
    #[test]
    fn quote_string_empty_string_tuple() {
        assert_eq!(
            from_str::<(String, String)>("\"test one two three\""),
            Ok((String::from("test one two three"), String::from("")))
        )
    }
    #[test]
    fn quote_string_followed_by_unquoted() {
        assert_eq!(
            from_str::<(String, String)>("\"test one two three\" a"),
            Ok((String::from("test one two three"), String::from("a")))
        )
    }
}
