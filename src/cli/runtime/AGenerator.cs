using System;
using System.Collections.Generic;
using System.Text;

namespace org.clojure.runtime
    {
public abstract class AGenerator : Iter{

    Object __val;
    int __state = 0;

    #region Iter Members

    public object get()
        {
        return __val;
        }

    abstract public Iter iterate();


    #endregion
    }
    }
