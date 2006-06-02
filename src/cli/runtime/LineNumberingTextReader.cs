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

namespace org.clojure.runtime
    {


public class LineNumberingTextReader : TextReader, IDisposable
    {
    TextReader impl;
    int line = 0;
    
    public LineNumberingTextReader(TextReader r){
        this.impl = r;
        }
    
	public int getLineNumber(){
		return line;
		}

    override public int Read(){
        int ret = impl.Read();
        if(ret == '\n')
            ++line;
        return ret;
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
