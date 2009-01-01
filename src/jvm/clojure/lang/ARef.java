/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jan 1, 2009 */

package clojure.lang;

public abstract class ARef extends AReference implements IRef {
    private volatile IFn validator = null;

    public ARef() {
        super();
    }

    public ARef(IPersistentMap meta) {
        super(meta);
    }

    void validate(IFn vf, Object val){
        try{
            if(vf != null && !RT.booleanCast(vf.invoke(val)))
                throw new IllegalStateException("Invalid reference state");
            }
        catch(RuntimeException re)
            {
            throw re;
            }
        catch(Exception e)
            {
            throw new IllegalStateException("Invalid reference state", e);
            }
    }

    void validate(Object val){
        validate(validator,val);
    }

    public void setValidator(IFn vf){
        try
            {
            validate(vf,get());
            }
        catch (Exception e)
            {
            throw new RuntimeException(e);
            }
        validator = vf;
    }

    public IFn getValidator(){
        return validator;
    }
}
