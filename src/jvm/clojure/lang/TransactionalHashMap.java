/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jul 31, 2007 */

package clojure.lang;

import java.util.*;
import java.util.concurrent.CyclicBarrier;

public class TransactionalHashMap<K, V> extends AbstractMap<K, V>{
final Ref mapref;

public TransactionalHashMap(){
	mapref = new Ref(PersistentHashMap.EMPTY);
}

public TransactionalHashMap(Map<? extends K, ? extends V> m){
	IPersistentMap map = PersistentHashMap.EMPTY;
	for(Entry<? extends K, ? extends V> e : m.entrySet())
		{
		map = map.assoc(e.getKey(), e.getValue());
		}
	mapref = new Ref(map);
}

public void commute(final K key, final IFn fn) throws Exception{
	mapref.commute(
			new AFn(){
				public Object invoke(Object arg1) throws Exception{
					IPersistentMap map = (IPersistentMap) arg1;
					V val = (V) map.valAt(key);
					return map.assoc(key, fn.invoke(val));
				}
			}
	);
}

public void commutePut(final K key, final V value){
	try
		{
		mapref.commute(
				new AFn(){
					public Object invoke(Object arg1) throws Exception{
						IPersistentMap map = (IPersistentMap) arg1;
						return map.assoc(key, value);
					}
				}
		);
		}
	catch(Exception e)
		{
		throw new RuntimeException(e);
		}
}

public void commuteRemove(final Object key){
	try
		{
		mapref.commute(
				new AFn(){

					public Object invoke(Object arg1) throws Exception{
						IPersistentMap map = (IPersistentMap) arg1;
						return map.without(key);
					}

					public Obj withMeta(IPersistentMap meta){
						throw new UnsupportedOperationException();
					}
				}
		);
		}
	catch(Exception e)
		{
		throw new RuntimeException(e);
		}
}

public void clear(){
	mapref.set(PersistentHashMap.EMPTY);
}

public boolean containsKey(Object key){
	return persistentMap().contains(key);
}

public V get(Object key){
	return (V) persistentMap().valAt(key);
}

public boolean isEmpty(){
	return persistentMap().count() == 0;
}

public V put(K key, V value){
	V ret = (V) persistentMap().valAt(key);
	mapref.set(persistentMap().assoc(key, value));
	return ret;
}

public V remove(Object key){
	V ret = (V) persistentMap().valAt(key);
	mapref.set(persistentMap().without(key));
	return ret;
}

public int size(){
	return persistentMap().count();
}

public IPersistentMap persistentMap(){
	return (IPersistentMap) mapref.get();
}

public Set<Entry<K, V>> entrySet(){
	return new AbstractSet<Entry<K, V>>(){

		public Iterator<Entry<K, V>> iterator(){
			return new Iterator<Entry<K, V>>(){

				ISeq seq = persistentMap().seq();
				Entry<K, V> last = null;

				public boolean hasNext(){
					return seq != null;
				}

				public Entry<K, V> next(){
					final IMapEntry e = (IMapEntry) seq.first();
					last =
							new Entry<K, V>(){

								public K getKey(){
									return (K) e.key();
								}

								public V getValue(){
									return (V) e.val();
								}

								public V setValue(V value){
									return put(getKey(), value);
								}

							};
					seq = seq.rest();
					return last;
				}

				public void remove(){
					mapref.set(persistentMap().without(last.getKey()));
				}
			};
		}

		public int size(){
			return persistentMap().count();
		}
	};
}
/*
public static void main(String[] args){
	try
		{
		if(args.length != 1)
			System.err.println("Usage: TransactionalHashMap n");
		final int n = Integer.parseInt(args[0]);
		final int[] xs = {3, 5, 7};
		final TransactionalHashMap<Integer, Integer>[] maps = new TransactionalHashMap[]{
				new TransactionalHashMap<Integer, Integer>(),
				new TransactionalHashMap<Integer, Integer>(),
				new TransactionalHashMap<Integer, Integer>()};

		LockingTransaction.runInTransaction(new AFn(){
			public Object invoke() throws Exception{
				for(int i = 0; i < n; i++)
					for(TransactionalHashMap<Integer, Integer> map : maps)
						map.put(i, i);
				return null;
			}
		});


		final CyclicBarrier barrier = new CyclicBarrier(xs.length + 1);

		for(int x : xs)
			{
			final int t = x;
			new Thread(new Runnable(){
				public void run(){
					try
						{
						System.err.println("Waiting to start");
						barrier.await();
						LockingTransaction.runInTransaction(new AFn(){
							public Object invoke() throws Exception{
								for(TransactionalHashMap<Integer, Integer> map : maps)
									{
									Iterator<Entry<Integer, Integer>> iter =
											map.entrySet().iterator();
									while(iter.hasNext())
										{
										Entry<Integer, Integer> e = iter.next();
										if(e.getKey() % t == 0)
											iter.remove();
										else
											e.setValue(e.getValue() * t);
										}
									System.out.printf("%s\n", map);
									}
								return null;
							}
						});
						System.err.println("Waiting to finish");
						barrier.await();
						}
					catch(Exception e)
						{
						throw new RuntimeException(e);
						}
				}
			}
			).start();
			}
		System.err.println("Waiting to start");
		barrier.await();
		System.err.println("Waiting to finish");
		barrier.await();

//		LockingTransaction.runInTransaction(new AFn(){
//			public Object invoke() throws Exception{
//				for(TransactionalHashMap<Integer, Integer> map : maps)
//					{
//					System.out.printf("%s\n", map);
//					}
//				return null;
//			}
//		});
		}
	catch(Exception e)
		{
		e.printStackTrace();
		}
}
*/
}
