using System;
namespace org.clojure.runtime
    {
    interface IObj
        {
        object put(IComparable key, object val);
        object get(IComparable key);
        bool has(IComparable key);
        }
    }
