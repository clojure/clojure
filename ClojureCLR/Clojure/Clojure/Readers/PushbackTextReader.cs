using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace clojure.lang
{
    public class PushbackTextReader : TextReader, IDisposable
    {
        #region Data

        protected TextReader _baseReader;
        protected TextReader BaseReader
        {
            get { return _baseReader; }
        }

        protected int _unreadChar;
        protected bool _hasUnread = false;

        #endregion

        #region C-tors

        public PushbackTextReader(TextReader reader)
        {
            _baseReader = reader;
        }

        #endregion

        #region Lookahead

        public override int Peek()
        {
            return _baseReader.Peek();
        }

        #endregion

        #region Unreading

        public virtual void Unread(int ch)
        {
            if (_hasUnread)
                throw new IOException("Can't unread a second character.");

            _unreadChar = ch;
            _hasUnread = true;
 
        }


        #endregion

        #region Basic reading

        public override int Read()
        {
            int ret;
            if (_hasUnread)
            {
                ret = _unreadChar;
                _hasUnread = false;
            }
            else
                ret = _baseReader.Read();

            return ret;
        }

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
