using System;
using System.Collections.Generic;
using System.Text;

namespace org.clojure.runtime
    {
public abstract class AGenerator : ISeq, Iter{

    Object __val;
    int __state = 0;



    #region ISeq Members

    public Iter iter()
        {
        //generators get 'primed' by calling iterate once, which pulls up to first yield
        return iterate();
        }

    #endregion

    #region Iter Members

    public object get()
        {
        return __val;
        }

    abstract public Iter iterate();


    #endregion
    }
    }
