using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using NUnit.Framework;

using clojure.lang;
using System.IO;

namespace Clojure.Tests.ReaderTests
{
    [TestFixture]
    public class LineNumberingTextReaderTests : AssertionHelper
    {
        const string _sample = "abc\nde\nfghijk\r\nlmnopq\n\nrstuv";
        StringReader _sr;
        LineNumberingTextReader _rdr;

        [SetUp]
        public void Setup()
        {
            _sr = new StringReader(_sample);
            _rdr = new LineNumberingTextReader(_sr);

        }

        [TearDown]
        public void TearDown()
        {
            _rdr.Close();
        }

        [Test]
        public void Initializes_properly()
        {
            Expect(_rdr.Position, EqualTo(0));
            Expect(_rdr.LineNumber, EqualTo(1));
            Expect(_rdr.Peek(), EqualTo((int)_sample[0]));
            Expect(_rdr.AtLineStart);
        }


        [Test]
        public void Reads_character_at_a_time()
        {
            int[] chars = new int[] { 
                'a', 'b', 'c', '\n', 
                'd', 'e', '\n', 
                'f', 'g', 'h', 'i', 'j', 'k', '\n', 
                'l', 'm', 'n', 'o', 'p', 'q', '\n', 
                '\n', 
                'r', 's', 't', 'u', 'v' };

            int[] positions = new int[] {
                1, 2, 3, 0,
                1, 2, 0,
                1, 2, 3, 4, 5, 6, 0,
                1, 2, 3, 4, 5, 6, 0,
                0,
                1, 2, 3, 4, 5 };

            int[] lines = new int[] {
                1, 1, 1, 2,
                2, 2, 3,
                3, 3, 3, 3, 3, 3, 4,
                4, 4, 4, 4, 4, 4, 5,
                6,
                6, 6, 6, 6, 6 };

            bool[] starts = new bool[] {
                false, false, false, true,
                false, false, true,
                false, false, false, false, false, false, true,
                false, false, false, false, false, false, true,
                true,
                false, false, false, false, false
            };

            int i=0;
            int ch;
            while ((ch = _rdr.Read()) != -1)
            {
                Expect(ch, EqualTo(chars[i]));
                Expect(_rdr.Position, EqualTo(positions[i]));
                Expect(_rdr.LineNumber, EqualTo(lines[i]));
                Expect(_rdr.AtLineStart, EqualTo(starts[i]));
                ++i;
            }
        }

        [Test]
        public void Reads_lines_at_a_time()
        {
            string[] lines = new string[] {
                "abc", 
                "de",
                "fghijk",
                "lmnopq",
                "",
                "rstuv" };

            int[] positions = new int[] {
                0,0,0,0,0,5 };

            int[] lineNums = new int[] {
                2, 3, 4, 5, 6, 6 };

            bool[] starts = new bool[] {
                true, true, true, true, true, true
            };

            int index = 0;
            string line;
            while ((line = _rdr.ReadLine()) != null)
            {
                Expect(line, EqualTo(lines[index]));
                Expect(_rdr.Position, EqualTo(positions[index]));
                Expect(_rdr.LineNumber, EqualTo(lineNums[index]));
                Expect(_rdr.AtLineStart, EqualTo(starts[index]));
                ++index;
            }
        }

        [Test]
        public void Reads_blocks_just_fine()
        {
            char[][] buffers = new char[][] {
                new char[] { 'a',  'b',  'c', '\n', 'd' },
                new char[] { 'e',  '\n', 'f', 'g',  'h' },
                new char[] { 'i',  'j',  'k', '\n', 'l' },
                new char[] { 'm',  'n',  'o', 'p',  'q' },
                new char[] { '\n', '\n', 'r', 's',  't' },
                new char[] { 'u',  'v' } };
            int[] positions = new int[] { 1, 3, 1, 6, 3, 5 };
            int[] lineNums = new int[] { 2, 3, 4, 4, 6, 6 };
            bool[] starts = new bool[] { false, false, false, false, false, true, };

            char[] buffer = new char[20];

            int index = 0;
            int count;
            while ((count = _rdr.Read(buffer, 0, 5)) != 0)
            {
                //Console.WriteLine("{0} {1}/{2} {3}/{4} {5}", index, _rdr.Position, positions[index], _rdr.LineNumber, lineNums[index], _rdr.AtLineStart);
                Expect(SameContents(buffer, buffers[index], count));
                Expect(_rdr.Position, EqualTo(positions[index]));
                Expect(_rdr.LineNumber, EqualTo(lineNums[index]));
                Expect(_rdr.AtLineStart, EqualTo(starts[index]));
                ++index;
            }

        }

        bool SameContents(char[] b1, char[] b2, int count)
        {
            for (int i = 0; i < count; i++)
                if (b1[i] != b2[i])
                    return false;

            return true;
        }


        [Test]
        [ExpectedException(typeof(IOException))]
        public void Double_unread_fails()
        {
            _rdr.Unread('a');
            _rdr.Unread('b');
        }

        [Test]
        public void Basic_unread_works()
        {
            int c1 = _rdr.Read();
            Expect(c1, EqualTo((int)'a'));
            Expect(_rdr.Position, EqualTo(1));
            Expect(_rdr.LineNumber, EqualTo(1));
            Expect(_rdr.AtLineStart, False);

            int c2 = _rdr.Read();
            Expect(c2, EqualTo((int)'b'));
            Expect(_rdr.Position, EqualTo(2));
            Expect(_rdr.LineNumber, EqualTo(1));
            Expect(_rdr.AtLineStart, False);

            _rdr.Unread('x');
            Expect(_rdr.Position, EqualTo(1));
            Expect(_rdr.LineNumber, EqualTo(1));
            Expect(_rdr.AtLineStart, False);

            int c3 = _rdr.Read();
            Expect(c3, EqualTo((int)'x'));
            Expect(_rdr.Position, EqualTo(2));
            Expect(_rdr.LineNumber, EqualTo(1));
            Expect(_rdr.AtLineStart, False);

            int c4 = _rdr.Read();
            Expect(c4, EqualTo((int)'c'));
            Expect(_rdr.Position, EqualTo(3));
            Expect(_rdr.LineNumber, EqualTo(1));
            Expect(_rdr.AtLineStart, False);
        }

        [Test]
        public void UnreadingNewlineWorks()
        {
            int c1 = _rdr.Read();
            Expect(c1, EqualTo((int)'a'));
            Expect(_rdr.Position, EqualTo(1));
            Expect(_rdr.LineNumber, EqualTo(1));
            Expect(_rdr.AtLineStart, False);

            int c2 = _rdr.Read();
            Expect(c2, EqualTo((int)'b'));
            Expect(_rdr.Position, EqualTo(2));
            Expect(_rdr.LineNumber, EqualTo(1));
            Expect(_rdr.AtLineStart, False);

            int c3 = _rdr.Read();
            Expect(c3, EqualTo((int)'c'));
            Expect(_rdr.Position, EqualTo(3));
            Expect(_rdr.LineNumber, EqualTo(1));
            Expect(_rdr.AtLineStart, False);

            int c4 = _rdr.Read();
            Expect(c4, EqualTo((int)'\n'));
            Expect(_rdr.Position, EqualTo(0));
            Expect(_rdr.LineNumber, EqualTo(2));
            Expect(_rdr.AtLineStart);

            _rdr.Unread(c4);
            Expect(_rdr.Position, EqualTo(3));
            Expect(_rdr.LineNumber, EqualTo(1));
            Expect(_rdr.AtLineStart, False);

            int c5 = _rdr.Read();
            Expect(c5, EqualTo((int)'\n'));
            Expect(_rdr.Position, EqualTo(0));
            Expect(_rdr.LineNumber, EqualTo(2));
            Expect(_rdr.AtLineStart);

            int c6 = _rdr.Read();
            Expect(c6, EqualTo((int)'d'));
            Expect(_rdr.Position, EqualTo(1));
            Expect(_rdr.LineNumber, EqualTo(2));
            Expect(_rdr.AtLineStart, False);
        }
    }
}
