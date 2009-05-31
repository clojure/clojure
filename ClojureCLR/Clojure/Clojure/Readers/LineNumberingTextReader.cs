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

namespace clojure.lang
{
    /// <summary>
    /// 
    /// </summary>
    public class LineNumberingTextReader : PushbackTextReader, IDisposable
    {
        #region Data

        private int _lineNumber = 1;
        public int LineNumber
        {
            get { return _lineNumber; }
        }


        private int _prevPosition = 0;
        private int _position = 0;
        public int Position
        {
            get { return _position; }
        }

        private bool _prevLineStart = true;
        private bool _atLineStart = true;
        public bool AtLineStart
        {
            get { return _atLineStart; }
        }


        #endregion

        #region c-tors

        public LineNumberingTextReader(TextReader reader)
            : base(reader)
        {
        }

        #endregion

        #region Basic reading

        public override int Read()
        {
            int ret = base.Read();

            _prevLineStart = _atLineStart;

            if (ret == -1)
            {
                _atLineStart = true;
                return ret;
            }

            _atLineStart = false;
            ++_position;

            if (ret == '\r')
            {
                if (Peek() == '\n')
                    ret = BaseReader.Read();
                else
                {
                    NoteLineAdvance();
                }
            }

            if ( ret == '\n' )
                NoteLineAdvance();

            return ret;
        }



        private void NoteLineAdvance()
        {
            _atLineStart = true;
            _lineNumber++;
            _prevPosition = _position - 1;
            _position = 0;
        }



        //public override int Read(char[] buffer, int index, int count)
        //{
        //    int numRead = _baseReader.Read(buffer, index, count);
        //    HandleLines(buffer, index, numRead);
        //    return numRead;
        //}

        //public override int ReadBlock(char[] buffer, int index, int count)
        //{
        //    int numRead =  _baseReader.ReadBlock(buffer, index, count);
        //    HandleLines(buffer, index, numRead);
        //    return numRead;
        //}

        //public override string ReadLine()
        //{
        //    string line = _baseReader.ReadLine();
        //    if (line != null)
        //    {
        //        _lineNumber++;
        //        _lastLinePosition = _position;
        //        _position = 0;
        //    }
        //    return line;
        //}

        //public override string ReadToEnd()
        //{
        //    string result =  _baseReader.ReadToEnd();
        //    HandleLines(result);
        //    return result;
        //}


        #endregion

        #region Unreading

        public override void Unread(int ch)
        {
            base.Unread(ch);

            --_position;

            if (ch == '\n')
            {
                --_lineNumber;
                _position = _prevPosition;
                _atLineStart = _prevLineStart;
            }
        }

        #endregion

        #region Counting lines

        //private void HandleLines(char[] buffer, int index, int numRead)
        //{
        //    for (int i = index; i < index + numRead; ++i)
        //        if (buffer[i] == '\n')
        //        {
        //            ++_lineNumber;
        //            _lastLinePosition = _position;
        //            _position = 0;
        //        }
        //        else
        //            ++_position;
        //}


        //private void HandleLines(string result)
        //{
        //    foreach (char c in result)
        //        if (c == '\n')
        //        {
        //            ++_lineNumber;
        //            _lastLinePosition = _position;
        //            _position = 0;
        //        }
        //        else
        //            ++_position;
        //}

        #endregion

        #region Lifetime methods

        public override void Close()
        {
            _baseReader.Close();
            base.Close();
        }

        void IDisposable.Dispose()
        {
            _baseReader.Dispose();
            base.Dispose();
        }

        #endregion
    }
}
