/**
 *   Copyright (c) David Miller. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;


using NUnit.Framework;
using Rhino.Mocks;

using clojure.lang;

using RMExpect = Rhino.Mocks.Expect;
using java.math;



namespace Clojure.Tests.LibTests
{
    [TestFixture]
    public class LispReaderTests : AssertionHelper
    {
        #region matchNumber tests

        [Test]
        public void MatchNumberMatchesZero()
        {
            object o1 = LispReader.matchNumber("0");
            object o2 = LispReader.matchNumber("-0");
            object o3 = LispReader.matchNumber("+0");

            Expect(o1, EqualTo(0));
            Expect(o2, EqualTo(0));
            Expect(o3, EqualTo(0));
        }

        [Test]
        public void MatchNumberMatchesDecimal()
        {
            object o1 = LispReader.matchNumber("123");
            object o2 = LispReader.matchNumber("+123");
            object o3 = LispReader.matchNumber("-123");
            object o4 = LispReader.matchNumber("123456789123456789123456789");

            Expect(o1, EqualTo(123));
            Expect(o2, EqualTo(123));
            Expect(o3, EqualTo(-123));
            Expect(o4, EqualTo(new BigInteger("123456789123456789123456789")));
        }

        [Test]
        public void MatchNumberMatchesHexadecimal()
        {
            object o1 = LispReader.matchNumber("0X12A");
            object o2 = LispReader.matchNumber("0xFFF");
            object o3 = LispReader.matchNumber("0xFFFFFFFFFFFFFFFFFFFFFFFF");

            Expect(o1, EqualTo(0x12A));
            Expect(o2, EqualTo(0xFFF));
            Expect(o3, EqualTo(new BigInteger("FFFFFFFFFFFFFFFFFFFFFFFF", 16)));
        }


        [Test]
        public void MatchNumberMatchesOctal()
        {
            object o1 = LispReader.matchNumber("0123");
            object o2 = LispReader.matchNumber("+0123");
            object o3 = LispReader.matchNumber("-0123");
            object o4 = LispReader.matchNumber("01234567012345670123456777");

            Expect(o1, EqualTo(83));
            Expect(o2, EqualTo(83));
            Expect(o3, EqualTo(-83));
            Expect(o4, EqualTo(new BigInteger("1234567012345670123456777", 8)));
        }

        [Test]
        public void MatchNumberMatchesSpecifiedRadix()
        {
            object o1 = LispReader.matchNumber("2R1100");
            object o2 = LispReader.matchNumber("4R123");
            object o3 = LispReader.matchNumber("-4R123");
            object o4 = LispReader.matchNumber("30R1234AQ");

            Expect(o1, EqualTo(12));
            Expect(o2, EqualTo(27));
            Expect(o3, EqualTo(-27));
            Expect(o4, EqualTo(new BigInteger("1234AQ", 30).longValue()));
        }

        [Test]
        public void MatchNumberMatchesFloats()
        {
            object o1 = LispReader.matchNumber("123.7");
            object o2 = LispReader.matchNumber("-123.7E4");
            object o3 = LispReader.matchNumber("+1.237e4");
            object o4 = LispReader.matchNumber("+1.237e-4");
            object o5 = LispReader.matchNumber("1.237e+4");

            Expect(o1, EqualTo(123.7));
            Expect(o2, EqualTo(-1237000.0));
            Expect(o3, EqualTo(1.237e4));
            Expect(o4, EqualTo(1.237e-4));
            Expect(o5, EqualTo(1.237e4));
        }

        [Test]
        public void MatchNumberMatchesDecimals()
        {
            object o1 = LispReader.matchNumber("123.7M");
            // MS implementation of BigDecimal parser does not allow these.
            //object o2 = LispReader.matchNumber("-123.7E4M");
            //object o3 = LispReader.matchNumber("+1.237e4M");
            //object o4 = LispReader.matchNumber("+1.237e-4M");
            //object o5 = LispReader.matchNumber("1.237e+4M");

            Expect(o1, EqualTo(new BigDecimal("123.7")));
            //Expect(o2, EqualTo(-1237000.0M));
            //Expect(o3, EqualTo(1.237e4M));
            //Expect(o4, EqualTo(1.237e-4M));
            //Expect(o5, EqualTo(1.237e4M));       
        }

        [Test]
        public void MatchNumberMatchesRatios()
        {
            object o1 = LispReader.matchNumber("12/1");
            object o2 = LispReader.matchNumber("12/4");
            object o3 = LispReader.matchNumber("12/5");
            object o4 = LispReader.matchNumber("12345678900000/123456789");

            Expect(o1, EqualTo(12));
            Expect(o2, EqualTo(3));
            Expect(o3, EqualTo(new Ratio(new BigInteger("12"),new BigInteger("5"))));
            Expect(o4, EqualTo(100000));
        }

        [Test]
        public void MatchNumberReadsWholeString()
        {
            object o1 = LispReader.matchNumber(" 123");
            object o2 = LispReader.matchNumber("123 ");
            object o3 = LispReader.matchNumber(" 12.3");
            object o4 = LispReader.matchNumber("12.3 ");
            object o5 = LispReader.matchNumber(" 1/23");
            object o6 = LispReader.matchNumber("1/23 ");

            Expect(o1, Null);
            Expect(o2, Null);
            Expect(o3, Null);
            Expect(o4, Null);
            Expect(o5, Null);
            Expect(o6, Null);
        }

        [Test]
        public void MatchNumberFailsToMatchWeirdThings()
        {
            object o1 = LispReader.matchNumber("123a");
            object o2 = LispReader.matchNumber("0x123Z");
            object o4 = LispReader.matchNumber("12.4/24.2");
            object o5 = LispReader.matchNumber("1.7M3");

            Expect(o1, Null);
            Expect(o2, Null);
            Expect(o4, Null);
            Expect(o5, Null);
        }


        [Test]
        [ExpectedException(typeof(java.lang.NumberFormatException))]
        public void MatchNumberFailsOnRadixSnafu()
        {
            object o3 = LispReader.matchNumber("10RAA");
        }
        #endregion

        #region Helpers

        static PushbackTextReader CreatePushbackReaderFromString(string s)
        {
            return new PushbackTextReader(new StringReader(s));
        }

        static object ReadFromString(string s)
        {
            return LispReader.read(CreatePushbackReaderFromString(s),true,null,false);
        }

        static LineNumberingTextReader CreateLNPBRFromString(string s)
        {
            return new LineNumberingTextReader(new StringReader(s));
        }

        static object ReadFromStringNumbering(string s)
        {
            return LispReader.read(CreateLNPBRFromString(s),true,null,false);
        }


        #endregion
        
        #region Testing EOF

        [Test]
        public void EofValueReturnedOnEof()
        {
            object o = LispReader.read(CreatePushbackReaderFromString("   "), false, 7, false);
            Expect(o, EqualTo(7));
        }

        [Test]
        [ExpectedException(typeof(System.IO.EndOfStreamException))]
        public void EofValueFailsOnEof()
        {
            object o = LispReader.read(CreatePushbackReaderFromString("   "), true, 7, false);
        }

        #endregion

        #region Testing a few numbers

        [Test]
        public void ReadReadsIntegers()
        {
            object o1 = ReadFromString("123");
            object o2 = ReadFromString("-123");
            object o3 = ReadFromString("+123");
            object o4 = ReadFromString("123456789123456789123456789");

            Expect(o1, EqualTo(123));
            Expect(o2, EqualTo(-123));
            Expect(o3, EqualTo(123));
            Expect(o4, EqualTo(new BigInteger("123456789123456789123456789")));
        }

        [Test]
        public void ReadReadsFloats()
        {
            object o1 = ReadFromString("123.4");
            object o2 = ReadFromString("-123.4E4");
            object o3 = ReadFromString("+123.4E-2");

            Expect(o1, EqualTo(123.4));
            Expect(o2, EqualTo(-123.4E4));
            Expect(o3, EqualTo(123.4E-2));
        }

        [Test]
        public void ReadReadsRatios()
        {
            object o1 = ReadFromString("123/456");
            object o2 = ReadFromString("-123/456");
            object o3 = ReadFromString("+123/456");

            Expect(o1, TypeOf(typeof(Ratio)));
            Expect(o2, TypeOf(typeof(Ratio)));
            Expect(o3, TypeOf(typeof(Ratio)));
        }



        #endregion

        #region Special tokens

        [Test]
        public void SlashAloneIsSlash()
        {
            object o = ReadFromString("/");
            Expect(o, TypeOf(typeof(Symbol)));
            Expect(((Symbol)o).Name, EqualTo("/"));
            Expect(((Symbol)o).Namespace, Null);
        }

        [Test]
        public void ClojureSlashIsSpecial()
        {
            object o = ReadFromString("clojure.core//");
            Expect(o, TypeOf(typeof(Symbol)));
            Expect(((Symbol)o).Name, EqualTo("/"));
            Expect(((Symbol)o).Namespace, EqualTo("clojure.core"));
        }

        [Test]
        public void TrueReturnsT()
        {
            object o = ReadFromString("true");
            Expect(o, TypeOf(typeof(bool)));
            Expect(o,EqualTo(true));
        }

        [Test]
        public void FalseReturnsF()
        {
            object o = ReadFromString("false");
            Expect(o, TypeOf(typeof(bool)));
            Expect(o, EqualTo(false));
        }

        [Test]
        public void NilIsNull()
        {
            object o = ReadFromString("nil");
            Expect(o, Null);
        }

        #endregion

        #region Symbolic tests

        [Test]
        public void ReadReadsSymbolWithNoNS()
        {
            object o1 = ReadFromString("abc");

            Expect(o1, TypeOf(typeof(Symbol)));
            Expect(((Symbol)o1).Name, EqualTo("abc"));
            Expect(((Symbol)o1).Namespace, Null);
        }

        [Test]
        public void ReadReadsSymbolWithNS()
        {
            object o1 = ReadFromString("ab/cd");

            Expect(o1, TypeOf(typeof(Symbol)));
            Expect(((Symbol)o1).Name, EqualTo("cd"));
            Expect(((Symbol)o1).Namespace, EqualTo("ab"));
        }

        [Test]
        public void TwoSlashesIsOkayApparently()
        {
            object o1 = ReadFromString("ab/cd/e");

            Expect(o1, TypeOf(typeof(Symbol)));
            Expect(((Symbol)o1).Name, EqualTo("e"));
            Expect(((Symbol)o1).Namespace, EqualTo("ab/cd"));
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void NamespaceEndingWithColonSlashIsBad()
        {
            object o1 = ReadFromString("ab:/cd");
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void NameEndingWithColonIsBad()
        {
            object o1 = ReadFromString("ab/cd:");
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void NameEndingWithColonIsBad2()
        {
            object o1 = ReadFromString("cd:");
        }

        [Test]
        public void NameMayContainMultipleColons()
        {
            object o1 = ReadFromString("a:b:c/d:e:f");
            Expect(o1, TypeOf(typeof(Symbol)));
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void NameContainingDoubleColonNotAtBeginningIsBad()
        {
            object o1 = ReadFromString("ab::cd");
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void NamespaceContainingDoubleColonNotAtBeginningIsBad()
        {
            object o1 = ReadFromString("ab::cd/ef");
        }

        #endregion

        #region Keyword tests

        [Test]
        public void LeadingColonIsKeyword()
        {
            object o1 = ReadFromString(":abc");
            Expect(o1, TypeOf(typeof(Keyword)));
            Expect(((Keyword)o1).Namespace, Null);
            Expect(((Keyword)o1).Name, EqualTo("abc"));
        }

        [Test]
        public void LeadingColonWithNSIsKeyword()
        {
            object o1 = ReadFromString(":ab/cd");
            Expect(o1, TypeOf(typeof(Keyword)));
            Expect(((Keyword)o1).Namespace, EqualTo("ab"));
            Expect(((Keyword)o1).Name, EqualTo("cd"));
        }

        // TODO: Add more tests dealing with :: resolution.

        [Test]
        public void LeadingDoubleColonMakesKeywordInCurrentNamespace()
        {
            object o1 = ReadFromString("::abc");
            Expect(o1, TypeOf(typeof(Keyword)));
            Expect(((Keyword)o1).Namespace, EqualTo(((Namespace)RT.CURRENT_NS.deref()).Name.Name));
            Expect(((Keyword)o1).Name, EqualTo("abc"));
        }

        // At one time, this test worked.  Now, according to the documentation, it should not work.  Did something change?  Never mind.
        //[Test]
        //public void LeadingDoubleColonDoesNotSetNamespaceIfPeriodsInName()
        //{
        //    object o1 = ReadFromString("::ab.cd");
        //    Expect(o1, TypeOf(typeof(Keyword)));
        //    Expect(((Keyword)o1).Namespace, Null);
        //    Expect(((Keyword)o1).Name, EqualTo("ab.cd"));
        //}

        #endregion

        #region String tests

        [Test]
        public void DoubleQuotesSurroundAString()
        {
            object o1 = ReadFromString("\"abc\"");
            Expect(o1,TypeOf(typeof(string)));
            Expect(o1, EqualTo("abc"));
        }

        [Test]
        [ExpectedException(typeof(System.IO.EndOfStreamException))]
        public void NoEndingDoubleQuoteFails()
        {
            object o1 = ReadFromString("\"abc");
        }

        [Test]
        public void EmptyStringWorks()
        {
            object o1 = ReadFromString("\"\"");
            Expect(o1, TypeOf(typeof(string)));
            Expect(o1, EqualTo(String.Empty));

        }

        [Test]
        public void EscapesWorkInStrings()
        {
            char[] chars = new char[] {
                '"', 'a', 
                '\\', 't', 'b',
                '\\', 'r', 'c',
                '\\', 'n', 'd',
                '\\', '\\', 'e',
                '\\', '"', 'f',
                '\\', 'b', 'g',
                '\\', 'f', 'h', '"' 
            };

            string s = new String(chars);
            Expect(s.Length, EqualTo(24));


            object o1 = ReadFromString(s);
            Expect(o1, EqualTo("a\tb\rc\nd\\e\"f\bg\fh"));
        }

        [Test]
        [ExpectedException(typeof(System.IO.EndOfStreamException))]
        public void EOFinEscapeIsError()
        {
            char[] chars = new char[] {
                '"', 'a', 
                '\\', 't', 'b',
                '\\', 'r', 'c',
                '\\', 'n', 'd',
                '\\' 
            };
            string s = new String(chars);

            object o1 = ReadFromString(s);
        }

        [Test]
        public void UnicodeEscapeInsertsUnicodeCharacter()
        {
                char[] chars = new char[] {
                '"', 'a', 
                '\\', 'u', '1', '2', 'C', '4',
                'b', '"'
            };

            string s = new String(chars);

            object o1 = ReadFromString(s);
            Expect(o1, EqualTo("a\u12C4b"));
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void UnicodeEscapeWithBadCharacterFails()
        {
            char[] chars = new char[] {
                '"', 'a', 
                '\\', 'u', '1', '2', 'X', '4',
                'b', '"'
            };
            string s = new String(chars);

            object o1 = ReadFromString(s);
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void UnicodeEscapeWithEOFFails()
        {
            char[] chars = new char[] {
                '"', 'a', 
                '\\', 'u', '1', '2', 'A', '"'
            };
            string s = new String(chars);

            object o1 = ReadFromString(s);
        }


        [Test]
        public void OctalEscapeInsertsCharacter()
        {
            char[] chars = new char[] {
                '"', 'a', 
                '\\', '1', '2',  '4',
                'b', '"'
            };
            string s = new String(chars);

            object o1 = ReadFromString(s);
            Expect(o1, EqualTo("a\x0054b"));  // hex/octal conversion
        }


        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void OctalEscapeWithBadDigitFails()
        {
            char[] chars = new char[] {
                '"', 'a', 
                '\\', '1', '8',  '4',
                'b', '"'
            };
            string s = new String(chars);

            object o1 = ReadFromString(s);
        }


        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void OctalEscapeWithEOFFails()
        {
            char[] chars = new char[] {
                '"', 'a', 
                '\\', '1', '8',  '"'
            };
            string s = new String(chars);

            object o1 = ReadFromString(s);
        }

        [ExpectedException(typeof(ArgumentException))]
        public void OctalEscapeOutOfRangeFails()
        {
            char[] chars = new char[] {
                '"', 'a', 
                '\\', '4', '7',  '7',
                'b', '"'
            };
            string s = new String(chars);

            object o1 = ReadFromString(s);
        }


        #endregion

        #region Character tests

        [Test]
        public void BackslashYieldsNextCharacter()
        {
            object o1 = ReadFromString("\\a");
            Expect(o1, TypeOf(typeof(Char)));
            Expect(o1, EqualTo('a'));
        }

        [Test]
        public void BackslashYieldsNextCharacterStoppingAtTerminator()
        {
            object o1 = ReadFromString("\\a b");
            Expect(o1, TypeOf(typeof(Char)));
            Expect(o1, EqualTo('a'));
        }

        [Test]
        [ExpectedException(typeof(System.IO.EndOfStreamException))]
        public void BackslashFollowedByEOFFails()
        {
            object o1 = ReadFromString("\\");
        }

        [Test]
        public void BackslashRecognizesSpecialNames()
        {
            object o1 = ReadFromString("\\newline");
            object o2 = ReadFromString("\\space");
            object o3 = ReadFromString("\\tab");
            object o4 = ReadFromString("\\backspace");
            object o5 = ReadFromString("\\formfeed");
            object o6 = ReadFromString("\\return");

            Expect(o1, EqualTo('\n'));
            Expect(o2, EqualTo(' '));
            Expect(o3, EqualTo('\t'));
            Expect(o4, EqualTo('\b'));
            Expect(o5, EqualTo('\f'));
            Expect(o6, EqualTo('\r'));
        }

        [Test]
        public void BackslashRecognizesUnicode()
        {
            object o1 = ReadFromString("\\u12C4");
            Expect(o1, EqualTo('\u12C4'));
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void BackslashUnicodeWithEOFFails()
        {
            object o1 = ReadFromString("\\u12C 4");
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void BackslashUnicodeInBadRangeFails()
        {
            object o1 = ReadFromString("\\uDAAA");
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void BackslashUnicodeWithBadDigitFails()
        {
            object o1 = ReadFromString("\\u12X4");
        }

        [Test]
        public void BackslashRecognizesOctal()
        {
            object o1 = ReadFromString("\\o124");
            Expect(o1, EqualTo('\x54'));
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void BackslashOctalWithEOFFails()
        {
            object o1 = ReadFromString("\\u12 4");
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void BackslashOctalInBadRangeFails()
        {
            object o1 = ReadFromString("\\o444");
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void BackslashOctalWithBadDigitFails()
        {
            object o1 = ReadFromString("\\o128");
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void BackslashOctalWithTooManyDigitsFails()
        {
            object o1 = ReadFromString("\\o0012 aa");
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void BackslashWithOtherFails()
        {
            object o1 = ReadFromString("\\aa");
        }

        #endregion

        #region comment tests

        [Test]
        public void SemicolonIgnoresToEndOfLine()
        {
            object o1 = ReadFromString("  ; ignore \n 123");
            Expect(o1, EqualTo(123));
        }

        [Test]
        public void SharpBangIgnoresToEndOfLine()
        {
            object o1 = ReadFromString("  #! ignore \n 123");
            Expect(o1, EqualTo(123));
        }

        #endregion

        #region Discard tests

        [Test]
        public void SharpUnderscoreIgnoresNextForm()
        {
            object o1 = ReadFromString("#_ (1 2 3) 4");
            Expect(o1, EqualTo(4));
        }

        [Test]
        public void SharpUnderscoreIgnoresNextFormInList()
        {
            object o1 = ReadFromString("( abc #_ (1 2 3) 12)");
            Expect(o1, TypeOf(typeof(PersistentList)));
            PersistentList pl = o1 as PersistentList;
            Expect(pl.count(), EqualTo(2));
            Expect(pl.first(), TypeOf(typeof(Symbol)));
            Expect(((Symbol)pl.first()).Name, EqualTo("abc"));
            Expect(((Symbol)pl.first()).Namespace, Null);
            Expect(pl.next().first(), TypeOf(typeof(int)));
            Expect(pl.next().first(), EqualTo(12));
            Expect(pl.next().next(), Null);
        }

        #endregion

        #region List tests

        [Test]
        public void CanReadBasicList()
        {
            Object o1 = ReadFromString("(abc 12)");
            Expect(o1, TypeOf(typeof(PersistentList)));
            PersistentList pl = o1 as PersistentList;
            Expect(pl.count(), EqualTo(2));
            Expect(pl.first(), TypeOf(typeof(Symbol)));
            Expect(((Symbol)pl.first()).Name, EqualTo("abc"));
            Expect(((Symbol)pl.first()).Namespace, Null);
            Expect(pl.next().first(), TypeOf(typeof(int)));
            Expect(pl.next().first(), EqualTo(12));
            Expect(pl.next().next(), Null);
        }

        [Test]
        public void CanReadEmptyList()
        {
            Object o1 = ReadFromString("(   )");
            Expect(o1, InstanceOfType(typeof(IPersistentList)));
            IPersistentList pl = o1 as IPersistentList;
            Expect(pl.count(), EqualTo(0));
        }

        [Test]
        public void CanReadNestedList()
        {
            Object o1 = ReadFromString("(a (b c) d)");
            Expect(o1, InstanceOfType(typeof(IPersistentList)));
            IPersistentList pl = o1 as IPersistentList;
            ISeq seq = pl.seq();
            Expect(pl.count(), EqualTo(3));
            Expect(seq.next().first(), InstanceOfType(typeof(IPersistentList)));
            IPersistentList sub = seq.next().first() as IPersistentList;
            Expect(sub.count(), EqualTo(2));
        }

        [Test]
        [ExpectedException(typeof(System.IO.EndOfStreamException))]
        public void MissingListTerminatorFails()
        {
            Object o1 = ReadFromString("(a b 1 2");
        }

        [Test]
        public void ListGetsLineNumber()
        {
            Object o1 = ReadFromStringNumbering("\n\n(a b \n1 2)");
            Expect(o1, InstanceOfType(typeof(IObj)));
            IObj io = o1 as IObj;
            Expect(io.meta().valAt(Keyword.intern(null,"line")), EqualTo(3));
        }

        #endregion

        #region VectorTests

        [Test]
        public void CanReadBasicVector()
        {
            Object o1 = ReadFromString("[abc 12]");
            Expect(o1, TypeOf(typeof(LazilyPersistentVector)));
            LazilyPersistentVector pl = o1 as LazilyPersistentVector;
            Expect(pl.count(), EqualTo(2));
            Expect(pl.nth(0), TypeOf(typeof(Symbol)));
            Expect(((Symbol)pl.nth(0)).Name, EqualTo("abc"));
            Expect(((Symbol)pl.nth(0)).Namespace, Null);
            Expect(pl.nth(1), TypeOf(typeof(int)));
            Expect(pl.nth(1), EqualTo(12));
        }

        [Test]
        public void CanReadEmptyVector()
        {
            Object o1 = ReadFromString("[   ]");
            Expect(o1, InstanceOfType(typeof(IPersistentVector)));
            IPersistentVector v = o1 as IPersistentVector;
            Expect(v.count(), EqualTo(0));
        }

        [Test]
        public void VectorCanContainNestedList()
        {
            Object o1 = ReadFromString("[a (b c) d]");
            Expect(o1, InstanceOfType(typeof(IPersistentVector)));
            IPersistentVector v = o1 as IPersistentVector;
            Expect(v.count(), EqualTo(3));
            Expect(v.nth(1), InstanceOfType(typeof(IPersistentList)));
            IPersistentList sub = v.nth(1) as IPersistentList;
            Expect(sub.count(), EqualTo(2));
        }

        [Test]
        public void VectorCanContainNestedVector()
        {
            Object o1 = ReadFromString("[a [b c] d]");
            Expect(o1, InstanceOfType(typeof(IPersistentVector)));
            IPersistentVector v = o1 as IPersistentVector;
            Expect(v.count(), EqualTo(3));
            Expect(v.nth(1), InstanceOfType(typeof(IPersistentVector)));
            IPersistentVector sub = v.nth(1) as IPersistentVector;
            Expect(sub.count(), EqualTo(2));
        }

        [Test]
        [ExpectedException(typeof(System.IO.EndOfStreamException))]
        public void MissingVectorTerminatorFails()
        {
            Object o1 = ReadFromString("[a b 1 2");
        }

        #endregion

        #region Map tests

        [Test]
        public void CanReadBasicMap()
        {
            Object o1 = ReadFromString("{:abc 12 14 a}");
            Expect(o1, InstanceOfType(typeof(IPersistentMap)));
            IPersistentMap m = o1 as IPersistentMap;
            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(Keyword.intern(null, "abc")), EqualTo(12));
            Expect(m.valAt(14), EqualTo(Symbol.intern("a")));
        }

        [Test]
        public void CanReadEmptyMap()
        {
            Object o1 = ReadFromString("{   }");
            Expect(o1, InstanceOfType(typeof(IPersistentMap)));
            IPersistentMap m = o1 as IPersistentMap;
            Expect(m.count(), EqualTo(0));
        }


        [Test]
        [ExpectedException(typeof(System.IO.EndOfStreamException))]
        public void MissingRightBraceFails()
        {
            Object o1 = ReadFromString("{a b 1 2");
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void MapWithOddNumberOfEntriesFails()
        {
            Object o1 = ReadFromString("{a b 1}");
        }



        #endregion

        #region Set tests

        [Test]
        public void CanReadBasicSet()
        {
            Object o1 = ReadFromString("#{abc 12}");
            Expect(o1, InstanceOfType(typeof(IPersistentSet)));
            IPersistentSet s = o1 as IPersistentSet;
            Expect(s.count(), EqualTo(2));
            Expect(s.contains(Symbol.intern("abc")));
            Expect(s.contains(12));
        }

        [Test]
        public void CanReadEmptySet()
        {
            Object o1 = ReadFromString("#{   }");
            Expect(o1, InstanceOfType(typeof(IPersistentSet)));
            IPersistentSet s = o1 as IPersistentSet;
            Expect(s.count(), EqualTo(0));
        }

        [Test]
        [ExpectedException(typeof(System.IO.EndOfStreamException))]
        public void MissingSetTerminatorFails()
        {
            Object o1 = ReadFromString("#{a b 1 2");
        }

        #endregion

        #region Unmatched delimiter tests

        [Test]
        [ExpectedException(typeof(Exception))]
        public void NakedRightParenIsBad()
        {
            Object o1 = ReadFromString("}");
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void NakedRightBracketIsBad()
        {
            Object o1 = ReadFromString("]");
        }


        [Test]
        [ExpectedException(typeof(Exception))]
        public void NakedRightBraceIsBad()
        {
            Object o1 = ReadFromString("}");
        }

        [Test]
        [ExpectedException(typeof(Exception))]
        public void MismatchedDelimiterIsBad()
        {
            Object o1 = ReadFromString("( a b c }");
        }

        #endregion

        #region  Wrapping forms

        [Test]
        public void QuoteWraps()
        {
            object o1 = ReadFromString("'a");
            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(2));
            Expect(s.first(),EqualTo(Symbol.intern("quote")));
            Expect(s.next().first(),TypeOf(typeof(Symbol)));
        }

        [Test]
        public void QuoteWraps2()
        {
            object o1 = ReadFromString("'(a b c)");
            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("quote")));
            Expect(s.next().first(), InstanceOfType(typeof(IPersistentList)));
            Expect(((IPersistentList)s.next().first()).count(), EqualTo(3));
        }


        [Test]
        public void MetaWraps()
        {
            object o1 = ReadFromString("^a");
            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("clojure.core","meta")));
            Expect(s.next().first(), TypeOf(typeof(Symbol)));
        }

        [Test]
        public void MetaWraps2()
        {
            object o1 = ReadFromString("^(a b c)");
            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("clojure.core", "meta")));
            Expect(s.next().first(), InstanceOfType(typeof(IPersistentList)));
            Expect(((IPersistentList)s.next().first()).count(), EqualTo(3));
        }

        [Test]
        public void DerefWraps()
        {
            object o1 = ReadFromString("@a");
            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("clojure.core", "deref")));
            Expect(s.next().first(), TypeOf(typeof(Symbol)));
        }

        [Test]
        public void DerefWraps2()
        {
            object o1 = ReadFromString("@(a b c)");
            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("clojure.core", "deref")));
            Expect(s.next().first(), InstanceOfType(typeof(IPersistentList)));
            Expect(((IPersistentList)s.next().first()).count(), EqualTo(3));
        }

        #endregion

        #region Syntax-quote tests

        [Test]
        public void SQOnSelfEvaluatingReturnsQuotedThing()
        {
            object o1 = ReadFromString("`:abc");
            object o2 = ReadFromString("`222");
            object o3 = ReadFromString("`\\a)");
            object o4 = ReadFromString("`\"abc\"");

            Expect(o1, TypeOf(typeof(Keyword)));
            Expect(o1, EqualTo(Keyword.intern(null, "abc")));
            Expect(o2, TypeOf(typeof(int)));
            Expect(o2, EqualTo(222));
            Expect(o3, TypeOf(typeof(char)));
            Expect(o3, EqualTo('a'));
            Expect(o4, TypeOf(typeof(string)));
            Expect(o4, EqualTo("abc"));
        }

        [Test]
        public void SQOnSpecialFormQuotes()
        {
            object o1 = ReadFromString("`def");
            Expect(o1,InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(),EqualTo(2) );
            Expect(s.first(),EqualTo(Symbol.intern("quote")));
            Expect(s.next().first(),InstanceOfType(typeof(Symbol)));
            Symbol sym = s.next().first() as Symbol;
            Expect(sym.Namespace, Null);
            Expect(sym.Name, EqualTo("def"));
        }

        [Test]
        public void SQOnRegularSymbolResolves()
        {
            object o1 = ReadFromString("`abc");
            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("quote")));
            Expect(s.next().first(), InstanceOfType(typeof(Symbol)));
            Symbol sym = s.next().first() as Symbol;
            Expect(sym.Namespace, EqualTo(((Namespace)RT.CURRENT_NS.deref()).Name.Name));
            Expect(sym.Name, EqualTo("abc"));
        }

        [Test]
        public void SQOnGensymGenerates()
        {
            object o1 = ReadFromString("`abc#");
            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("quote")));
            Expect(s.next().first(), InstanceOfType(typeof(Symbol)));
            Symbol sym = s.next().first() as Symbol;
            Expect(sym.Namespace, Null);
            Expect(sym.Name.StartsWith("abc_")); ;
        }


        [Test]
        public void SQOnGensymSeesSameTwice()
        {
            object o1 = ReadFromString("`(abc# abc#)");
            // Return should be 
            //    (clojure/seq (clojure/concat (clojure/list (quote abc__N)) 
            //                                 (clojure/list (quote abc__N)))))
            string str = o1.ToString();

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(),EqualTo(2));
            Expect(s.first(),EqualTo(Symbol.intern("clojure.core","seq")));
            Expect(s.next().first(),InstanceOfType(typeof(ISeq)));
            ISeq s1 = s.next().first() as ISeq;

            Expect(s1.count(), EqualTo(3));
            Expect(s1.first(), EqualTo(Symbol.intern("clojure.core","concat")));

            Expect(s1.next().first(), InstanceOfType(typeof(ISeq)));
            ISeq s2 = s1.next().first() as ISeq;
            Expect(s2.count(), EqualTo(2));

            Expect(s2.next().first(), InstanceOfType(typeof(ISeq)));
            ISeq s2a = s2.next().first() as ISeq;
            Expect(s2a.next().first(), InstanceOfType(typeof(Symbol)));
            Symbol sym1 = s2a.next().first() as Symbol;

            Expect(s1.next().next().first(), InstanceOfType(typeof(ISeq)));
            ISeq s3 = s1.next().next().first() as ISeq;
            Expect(s3.count(), EqualTo(2));

            Expect(s3.next().first(), InstanceOfType(typeof(ISeq)));
            ISeq s3a = s3.next().first() as ISeq;
            Expect(s3a.next().first(), InstanceOfType(typeof(Symbol)));
            Symbol sym2 = s3a.next().first() as Symbol;

            Expect(sym1.Namespace, Null);
            Expect(sym1.Name.StartsWith("abc__"));
            Expect(sym1, EqualTo(sym2));
        }

        [Test]
        public void SQOnMapMakesMap()
        {
            Object o1 = ReadFromString("`{:a 1 :b 2}");
            //  (clojure/apply 
            //      clojure/hash-map 
            //         (clojure/seq 
            //             (clojure/concat (clojure/list :a) 
            //                             (clojure/list 1) 
            //                             (clojure/list :b) 
            //                             (clojure/list 2))))

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(3));
            Expect(s.first(), EqualTo(Symbol.intern("clojure.core/apply")));
            Expect(s.next().first(), EqualTo(Symbol.intern("clojure.core/hash-map")));
            Expect(s.next().next().first(), InstanceOfType(typeof(ISeq)));

            ISeq s0 = s.next().next().first() as ISeq;
            ISeq s2;

            Expect(s0.count(), EqualTo(2));
            Expect(s0.first(), EqualTo(Symbol.intern("clojure.core/seq")));
            Expect(s0.next().first(),InstanceOfType(typeof(ISeq)));
            ISeq s1 = s0.next().first() as ISeq;

            Expect(s1.count(), EqualTo(5));
            Expect(s1.first(), EqualTo(Symbol.intern("clojure.core/concat")));

            s1 = s1.next();
            Expect(s1.first(),InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(),EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(),EqualTo(Keyword.intern(null,"a")));


            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(), EqualTo(1));

            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(), EqualTo(Keyword.intern(null, "b")));


            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(), EqualTo(2));
        }

        public void SQOnVectorMakesVector()
        {
            Object o1 = ReadFromString("`[:b 2]");
            //  (clojure/apply 
            //      clojure/vector 
            //         (clojure/concat (clojure/list :b) 
            //                         (clojure/list 2)))

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(3));
            Expect(s.first(), EqualTo(Symbol.intern("clojure.core/apply")));
            Expect(s.next().first(), EqualTo(Symbol.intern("clojure.core/vector")));
            Expect(s.next().next().first(), InstanceOfType(typeof(ISeq)));

            ISeq s1 = s.next().next().first() as ISeq;
            ISeq s2;

            Expect(s1.count(), EqualTo(3));
            Expect(s1.first(), EqualTo(Symbol.intern("clojure.core/concat")));

            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(), EqualTo(Keyword.intern(null, "b")));


            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(), EqualTo(2));
        }

        [Test]
        public void SQOnSetMakesSet()
        {
            Object o1 = ReadFromString("`#{:b 2}");
            //  (clojure/apply 
            //      clojure/hash-set 
            //         (clojure/seq
            //             (clojure/concat (clojure/list :b) 
            //                             (clojure/list 2))))

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(3));
            Expect(s.first(), EqualTo(Symbol.intern("clojure.core/apply")));
            Expect(s.next().first(), EqualTo(Symbol.intern("clojure.core/hash-set")));
            Expect(s.next().next().first(), InstanceOfType(typeof(ISeq)));

            ISeq s0 = s.next().next().first() as ISeq;
            Expect(s0.count(), EqualTo(2));
            Expect(s0.first(),EqualTo(Symbol.intern("clojure.core/seq")));
            Expect(s0.next().first(),InstanceOfType(typeof(ISeq)));

            ISeq s1 = s0.next().first() as ISeq;
            
            Expect(s1.count(), EqualTo(3));
            Expect(s1.first(), EqualTo(Symbol.intern("clojure.core/concat")));

            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));
            ISeq s2 = s1.first() as ISeq;
            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));
            ISeq s3 = s1.first() as ISeq;

            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s3.first(), EqualTo(Symbol.intern("clojure.core/list")));


            object e1 = s2.next().first();
            object e2 = s3.next().first();

            // Set elements can occur in any order

            Expect(e1, EqualTo(Keyword.intern(null, "b")) | EqualTo(2));
            Expect(e2, EqualTo(Keyword.intern(null, "b")) | EqualTo(2));
            Expect(e1, Not.EqualTo(e2));
        }

        [Test]
        public void SQOnListMakesList()
        {
            Object o1 = ReadFromString("`(:b 2)");
            //   (clojure/seq (clojure/concat (clojure/list :b) 
            //                                (clojure/list 2))))

            Expect(o1, InstanceOfType(typeof(ISeq)));

            ISeq s0 = o1 as ISeq;
            Expect(s0.count(), EqualTo(2));
            Expect(s0.first(), EqualTo(Symbol.intern("clojure.core/seq")));
            Expect(s0.next().first(),InstanceOfType(typeof(ISeq)));
            ISeq s1 = s0.next().first() as ISeq;
            ISeq s2;

            Expect(s1.count(), EqualTo(3));
            Expect(s1.first(), EqualTo(Symbol.intern("clojure.core/concat")));

            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(), EqualTo(Keyword.intern(null, "b")));


            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(), EqualTo(2));
        }


        [Test]
        public void UnquoteStandaloneReturnsUnquoteObject()
        {
            object o1 = ReadFromString("~x");

            //Expect(o1, InstanceOfType(typeof(LispReader.Unquote)));
            //LispReader.Unquote u = o1 as LispReader.Unquote;
            //Expect(u.Obj, EqualTo(Symbol.intern("x")));
            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.first(), EqualTo(Symbol.intern("clojure.core/unquote")));
            Expect(s.next().first(), EqualTo(Symbol.intern("x")));
            Expect(s.count(), EqualTo(2));

        }

        [Test]
        public void UnquoteSpliceStandaloneReturnsUnquoteSpliceObject()
        {
            object o1 = ReadFromString("~@x");

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.first(), EqualTo(Symbol.intern("clojure.core/unquote-splicing")));
            Expect(s.next().first(), EqualTo(Symbol.intern("x")));
            Expect(s.count(), EqualTo(2));
        }

        [Test]
        public void SQonUnquoteDequotes()
        {
            object o1 = ReadFromString("`(a ~b)");
            // (clojure/seq (clojure/concat (clojure/list (quote NS/a)) 
            //                              (clojure/list b)))


            Expect(o1, InstanceOfType(typeof(ISeq)));

            ISeq s0 = o1 as ISeq;
            Expect(s0.count(),EqualTo(2));
            Expect(s0.first(),EqualTo(Symbol.intern("clojure.core/seq")));
            Expect(s0.next().first(),InstanceOfType(typeof(ISeq)));

            ISeq s1 = s0.next().first() as ISeq;
            ISeq s2;

            Expect(s1.count(), EqualTo(3));
            Expect(s1.first(), EqualTo(Symbol.intern("clojure.core/concat")));

            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(), InstanceOfType(typeof(ISeq)));
            ISeq s3 = s2.next().first() as ISeq;

            Expect(s3.count(), EqualTo(2));
            Expect(s3.first(), EqualTo(Symbol.intern("quote")));
            Expect(s3.next().first(), EqualTo(Symbol.intern(((Namespace)RT.CURRENT_NS.deref()).Name.Name,"a")));


            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(), EqualTo(Symbol.intern("b")));
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void SQonUnquoteSpliceNotInListFails()
        {
            object o1 = ReadFromString("`~@x");
        }

        [Test]
        public void SqOnUnquoteSpliceSplices()
        {
            object o1 = ReadFromString("`(a ~@b)");
            // (clojure/seq (clojure/concat (clojure/list (quote user/a)) b))

            Expect(o1, InstanceOfType(typeof(ISeq)));

            ISeq s0 = o1 as ISeq;
            Expect(s0.count(), EqualTo(2));
            Expect(s0.first(), EqualTo(Symbol.intern("clojure.core/seq")));
            Expect(s0.next().first(), InstanceOfType(typeof(ISeq)));

            ISeq s1 = s0.next().first() as ISeq;
            ISeq s2;

            Expect(s1.count(), EqualTo(3));
            Expect(s1.first(), EqualTo(Symbol.intern("clojure.core/concat")));

            s1 = s1.next();
            Expect(s1.first(), InstanceOfType(typeof(ISeq)));

            s2 = s1.first() as ISeq;
            Expect(s2.first(), EqualTo(Symbol.intern("clojure.core/list")));
            Expect(s2.next().first(), InstanceOfType(typeof(ISeq)));
            ISeq s3 = s2.next().first() as ISeq;

            Expect(s3.count(), EqualTo(2));
            Expect(s3.first(), EqualTo(Symbol.intern("quote")));
            Expect(s3.next().first(), EqualTo(Symbol.intern(((Namespace)RT.CURRENT_NS.deref()).Name.Name, "a")));


            s1 = s1.next();
            Expect(s1.first(), EqualTo(Symbol.intern("b")));
         }

        // We should test to see that 'line' meta info is not preserved.

        [Test]
        public void SQOnLparenRParenReturnsEmptyList()
        {
            object o1 = ReadFromString("`()");
            //  (clojure/list)
            Expect(o1,InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(),EqualTo(1));
            Expect(s.first(),EqualTo(Symbol.intern("clojure.core/list")));
        }

        #endregion

        #region #-dispatch tests

        [Test]
        [ExpectedException(typeof(Exception))]
        public void SharpDispatchOnInvalidCharFails()
        {
            object o1 = ReadFromString("#a(1 2)");
        }

        #endregion

        #region Meta reader tests

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void MetaOnImproperMetadataFails()
        {
            object o1 = ReadFromString("#^1 (a b c");
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void MetaOnAppliedToNonIObjFails()
        {
            object o1 = ReadFromString("#^{:a 1} 7");
        }

        [Test]
        public void MetaAppliesHashMetaDataToObject()
        {
            object o1 = ReadFromString("#^{a 1} (a b)");

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;

            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("a")));
            Expect(s.next().first(), EqualTo(Symbol.intern("b")));

            Expect(o1, InstanceOfType(typeof(IObj)));
            IObj o = o1 as IObj;
            Expect(o.meta(), InstanceOfType(typeof(IPersistentMap)));
            IPersistentMap m = o.meta() as IPersistentMap;

            Expect(m.count(), EqualTo(1));
            Expect(m.valAt(Symbol.intern("a")), EqualTo(1));
        }


        [Test]
        public void MetaAppliesSymbolAsTagMetaDataToObject()
        {
            object o1 = ReadFromString("#^c (a b)");

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;

            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("a")));
            Expect(s.next().first(), EqualTo(Symbol.intern("b")));

            Expect(o1, InstanceOfType(typeof(IObj)));
            IObj o = o1 as IObj;
            Expect(o.meta(), InstanceOfType(typeof(IPersistentMap)));
            IPersistentMap m = o.meta() as IPersistentMap;

            Expect(m.count(), EqualTo(1));
            Expect(m.valAt(Keyword.intern(null,"tag")), EqualTo(Symbol.intern("c")));
        }

        [Test]
        public void MetaAppliesKeywordAsTagMetaDataToObject()
        {
            object o1 = ReadFromString("#^:c (a b)");

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;

            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("a")));
            Expect(s.next().first(), EqualTo(Symbol.intern("b")));

            Expect(o1, InstanceOfType(typeof(IObj)));
            IObj o = o1 as IObj;
            Expect(o.meta(), InstanceOfType(typeof(IPersistentMap)));
            IPersistentMap m = o.meta() as IPersistentMap;

            Expect(m.count(), EqualTo(1));
            Expect(m.valAt(Keyword.intern(null, "tag")), EqualTo(Keyword.intern(null,"c")));
        }

        [Test]
        public void MetaAppliesStringAsTagMetaDataToObject()
        {
            object o1 = ReadFromString("#^\"help\" (a b)");

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;

            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("a")));
            Expect(s.next().first(), EqualTo(Symbol.intern("b")));

            Expect(o1, InstanceOfType(typeof(IObj)));
            IObj o = o1 as IObj;
            Expect(o.meta(), InstanceOfType(typeof(IPersistentMap)));
            IPersistentMap m = o.meta() as IPersistentMap;

            Expect(m.count(), EqualTo(1));
            Expect(m.valAt(Keyword.intern(null, "tag")), EqualTo("help"));
        }

        [Test]
        public void MetaAddsLineupNumberAsMetaDataIfAvailable()
        {
            object o1 = ReadFromStringNumbering("\n\n#^:c (a b)");

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;

            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("a")));
            Expect(s.next().first(), EqualTo(Symbol.intern("b")));

            Expect(o1, InstanceOfType(typeof(IObj)));
            IObj o = o1 as IObj;
            Expect(o.meta(), InstanceOfType(typeof(IPersistentMap)));
            IPersistentMap m = o.meta() as IPersistentMap;

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(Keyword.intern(null, "tag")), EqualTo(Keyword.intern(null,"c")));
            Expect(m.valAt(Keyword.intern(null, "line")), EqualTo(3));
        }

        #endregion

        #region Var reader tests

        [Test]
        public void VarWrapsVar()
        {
            Object o1 = ReadFromString("#'abc");

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;
            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(Symbol.intern("var")));
            Expect(s.next().first(), EqualTo(Symbol.intern("abc")));
        }

        #endregion

        #region Regex reader tests

        [Test]
        public void SharpDoubleQuoteGeneratesRegex()
        {
            object o1 = ReadFromString("#\"abc\"");

            Expect(o1, InstanceOfType(typeof(System.Text.RegularExpressions.Regex)));
            System.Text.RegularExpressions.Regex r = o1 as System.Text.RegularExpressions.Regex;
            Expect(r.ToString(), EqualTo("abc"));
        }

        [Test]
        [ExpectedException(typeof(System.IO.EndOfStreamException))]
        public void SharpDQHitsEOFFails()
        {
            object o1 = ReadFromString("#\"abc");
        }

        [Test]
        public void SharpDQEscapesOnBackslash()
        {
            char[] chars = new char[] {
                '#', '"', 'a', '\\', '"', 'b', 'c', '"'
            };

            //  input =  #"a\"bc" -- should go over the "

            string str = new String(chars);

            object o1 = ReadFromString(str);

            Expect(o1, InstanceOfType(typeof(System.Text.RegularExpressions.Regex)));
            System.Text.RegularExpressions.Regex r = o1 as System.Text.RegularExpressions.Regex;
            Expect(r.ToString(), EqualTo("a\\\"bc"));
        }

        #endregion

        #region Fn reader & Arg reader tests

        [Test]
        public void SharpFnWithNoArgsGeneratesNoArgFn()
        {
            object o1 = ReadFromString("#(+ 1 2)");
            // (fn* [] (+ 1 2))

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;

            Expect(s.first(), EqualTo(Symbol.intern("fn*")));
            s = s.next();
            Expect(s.first(), InstanceOfType(typeof(IPersistentVector)));
            IPersistentVector arglist = s.first() as IPersistentVector;

            Expect(arglist.count(), EqualTo(0));

            s = s.next();
            Expect(s.first(), InstanceOfType(typeof(ISeq)));
            Expect(s.next(), Null);

            ISeq form = s.first() as ISeq;

            Expect(form.count(), EqualTo(3));
            Expect(form.first(), EqualTo(Symbol.intern("+")));
            Expect(form.next().first(), EqualTo(1));
            Expect(form.next().next().first(), EqualTo(2));
        }

        [Test]
        public void SharpFnWithArgsGeneratesFnWithArgs()
        {
            object o1 = ReadFromString("#(+ %2 2)");
            // (fn* [p1__N p2__M] (+ p2__M 2))

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;

            Expect(s.first(), EqualTo(Symbol.intern("fn*")));
            s = s.next();
            Expect(s.first(), InstanceOfType(typeof(IPersistentVector)));
            IPersistentVector arglist = s.first() as IPersistentVector;

            Expect(arglist.count(), EqualTo(2));
            Expect(arglist.nth(0), InstanceOfType(typeof(Symbol)));
            Expect(arglist.nth(1), InstanceOfType(typeof(Symbol)));
            Symbol arg1 = arglist.nth(0) as Symbol;
            Symbol arg2 = arglist.nth(1) as Symbol;
            Expect(arg1.Name, StartsWith("p1__"));
            Expect(arg2.Name, StartsWith("p2__"));

            s = s.next();
            Expect(s.first(), InstanceOfType(typeof(ISeq)));
            Expect(s.next(), Null);

            ISeq form = s.first() as ISeq;

            Expect(form.count(), EqualTo(3));
            Expect(form.first(), EqualTo(Symbol.intern("+")));
            Expect(form.next().first(), EqualTo(arg2));
            Expect(form.next().next().first(), EqualTo(2));
        }

        [Test]
        public void SharpFnWithRestArgGeneratesFnWithRestArg()
        {
            object o1 = ReadFromString("#(+ %2 %&)");
            // (fn* [p1__N p2__M & rest__X] (+ p2__M rest__X))

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;

            Expect(s.first(), EqualTo(Symbol.intern("fn*")));
            s = s.next();
            Expect(s.first(), InstanceOfType(typeof(IPersistentVector)));
            IPersistentVector arglist = s.first() as IPersistentVector;

            Expect(arglist.count(), EqualTo(4));
            Expect(arglist.nth(0), InstanceOfType(typeof(Symbol)));
            Expect(arglist.nth(1), InstanceOfType(typeof(Symbol)));
            Expect(arglist.nth(2), InstanceOfType(typeof(Symbol)));
            Expect(arglist.nth(3), InstanceOfType(typeof(Symbol)));
            Symbol arg1 = arglist.nth(0) as Symbol;
            Symbol arg2 = arglist.nth(1) as Symbol;
            Symbol arg3 = arglist.nth(2) as Symbol;
            Symbol arg4 = arglist.nth(3) as Symbol;
            Expect(arg1.Name, StartsWith("p1__"));
            Expect(arg2.Name, StartsWith("p2__"));
            Expect(arg3.Name, EqualTo("&"));
            Expect(arg4.Name, StartsWith("rest__"));

            s = s.next();
            Expect(s.first(), InstanceOfType(typeof(ISeq)));
            Expect(s.next(), Null);

            ISeq form = s.first() as ISeq;

            Expect(form.count(), EqualTo(3));
            Expect(form.first(), EqualTo(Symbol.intern("+")));
            Expect(form.next().first(), EqualTo(arg2));
            Expect(form.next().next().first(), EqualTo(arg4));
        }

        [Test]
        public void SharpFnWithAnonArgGeneratesFnWithArgs()
        {
            object o1 = ReadFromString("#(+ % 2)");
            // (fn* [p1__N] (+ p1__N 2))

            Expect(o1, InstanceOfType(typeof(ISeq)));
            ISeq s = o1 as ISeq;

            Expect(s.first(), EqualTo(Symbol.intern("fn*")));
            s = s.next();
            Expect(s.first(), InstanceOfType(typeof(IPersistentVector)));
            IPersistentVector arglist = s.first() as IPersistentVector;

            Expect(arglist.count(), EqualTo(1));
            Expect(arglist.nth(0), InstanceOfType(typeof(Symbol)));
            Symbol arg1 = arglist.nth(0) as Symbol;
            Expect(arg1.Name, StartsWith("p1__"));

            s = s.next();
            Expect(s.first(), InstanceOfType(typeof(ISeq)));
            Expect(s.next(), Null);

            ISeq form = s.first() as ISeq;

            Expect(form.count(), EqualTo(3));
            Expect(form.first(), EqualTo(Symbol.intern("+")));
            Expect(form.next().first(), EqualTo(arg1));
            Expect(form.next().next().first(), EqualTo(2));
        }

        [Test]
        [ExpectedException(typeof(InvalidOperationException))]
        public void ArgReaderOutsideSharpFnFails()
        {
            object o1 = ReadFromString("(+ %2 2)");
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void ArgReaderFollowedByNonNumericFails()
        {
            object o1 = ReadFromString("#(+ %a 2)");
        }



        #endregion

        #region Eval reader tests

        // TODO: EvalReader tests


        #endregion

    }
}
