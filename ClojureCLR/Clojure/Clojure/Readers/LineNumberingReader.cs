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

namespace clojure.lang.Readers
{
    public class LineNumberingReader : TextReader
    {
        #region Data

        private TextReader _baseReader;

        protected TextReader BaseReader
        {
            get { return _baseReader; }
        }


        private int _lineNumber = 1;

        public int LineNumber
        {
            get { return _lineNumber; }
        }


        private int _position = 0;

        protected int Position
        {
            get { return _position; }
        }

        #endregion

        #region c-tors

        public LineNumberingReader(TextReader reader)
        {
            _baseReader = reader;
        }

        #endregion

        #region Lifetime methods

        public override void Close()
        {
            _baseReader.Close();
            base.Close();
        }

        #endregion

        #region Lookahead

        public override int Peek()
        {
            return _baseReader.Peek();
        }

        #endregion

        #region Basic reading

        public override int Read()
        {
            int ret = _baseReader.Read();
            switch (ret)
            {
                case '\n':
                    _lineNumber++;
                    _position = 0;
                    break;

                case '\r':
                    if (Peek() == '\n')
                    {
                        ret = _baseReader.Read();
                        goto case '\n';
                    }
                    break;
            }
            return ret;
        }

        public override int Read(char[] buffer, int index, int count)
        {
            int numRead = _baseReader.Read(buffer, index, count);
            HandleLines(buffer, index, numRead);
            return numRead;
        }

        public override int ReadBlock(char[] buffer, int index, int count)
        {
            int numRead =  _baseReader.ReadBlock(buffer, index, count);
            HandleLines(buffer, index, numRead);
            return numRead;
        }



        public override string ReadLine()
        {
            string line = _baseReader.ReadLine();
            if (line != null)
            {
                _lineNumber++;
                _position = 0;
            }
            return line;
        }

        public override string ReadToEnd()
        {
            string result =  _baseReader.ReadToEnd();
            HandleLines(result);
            return result;
        }


        #endregion

        #region Counting lines

        private void HandleLines(char[] buffer, int index, int numRead)
        {
            for (int i = index; i < index + numRead; ++i)
                if (buffer[i] == '\n')
                {
                    ++_lineNumber;
                    _position = 0;
                }
                else
                    ++_position;
        }


        private void HandleLines(string result)
        {
            foreach (char c in result)
                if (c == '\n')
                {
                    ++_lineNumber;
                    _position = 0;
                }
                else
                    ++_position;
        }

        #endregion
    }
}
