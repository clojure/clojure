/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.IO;

namespace clojure.lang
    {


public class LineNumberingTextReader : TextReader, IDisposable
    {
    TextReader impl;
    int line = 0;
    int unreadChar;
    bool haveUnread = false;
    
    public LineNumberingTextReader(TextReader r){
        this.impl = r;
        }
    
	public int getLineNumber(){
		return line;
		}

    override public int Read(){
        int ret;
        if(haveUnread)
			{
			ret = unreadChar;
			haveUnread = false;
			}
		else
			ret = impl.Read();
        if(ret == '\n')
            ++line;
        return ret;
        }
        
    public void unread(int ch){
		if(haveUnread)
			throw new InvalidOperationException("Can't unread more than once in a row");
		unreadChar = ch;
		haveUnread = true;
		if (ch == '\n')
			--line;
		}

    override public int Peek(){
        return impl.Peek();
        }

    public override void Close()
        {
        base.Close();
        impl.Close();
        }

	void IDisposable.Dispose()
		{
		base.Dispose();
		impl.Dispose();
		} 
    
    }


    }
