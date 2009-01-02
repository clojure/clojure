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

import java.util.concurrent.atomic.AtomicReference;
import java.util.Map;

public abstract class ARef extends AReference implements IRef {
    protected volatile IFn validator = null;
    private AtomicReference<IPersistentMap> watchers = new AtomicReference(PersistentHashMap.EMPTY);

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

    public IPersistentMap getWatches(){
        return watchers.get();
    }
    
    public IRef addWatch(Agent watcher, IFn action, boolean sendOff){
        boolean added = false;
        IPersistentMap prior = null;
        while(!added)
            {
            prior = watchers.get();
            added = watchers.compareAndSet(prior, prior.assoc(watcher,new Object[]{action,sendOff}));
            }

        return this;
    }

    public IRef removeWatch(Agent watcher){
        boolean removed = false;
        IPersistentMap prior = null;
        while(!removed)
            {
            prior = watchers.get();
            try
                {
                removed = watchers.compareAndSet(prior, prior.without(watcher));
                }
            catch (Exception e)
                {
                throw new RuntimeException(e);
                }
            }

        return this;
    }

    public void notifyWatches() {
        IPersistentMap ws = watchers.get();
        if (ws != null)
            {
            ISeq args = new Cons(this, null);
            for (ISeq s = RT.seq(ws); s != null; s = s.rest())
                {
                Map.Entry e = (Map.Entry) s.first();
                Object[] a = (Object[]) e.getValue();
                Agent agent = (Agent) e.getKey();
                try
                    {
                    agent.dispatch((IFn) a[0], args, (Boolean)a[1]);
                    }
                catch (Exception e1)
                    {
                    //eat dispatching exceptions and continue
                    }
                }
            }
    }
}
