/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich May 20, 2006 */

using System;
using System.Collections;
using System.Collections.Specialized;

namespace org.clojure.runtime
{

/**
 * Persistent Red Black Tree
 * Note that instances of this class are constant values
 * i.e. add/remove etc return new values
 * <p/>
 * See Okasaki, Kahrs, Larsen et al
 */

public class PersistentTree : IPersistentMap, ISequential{

public readonly IComparer comp;
public readonly Node tree;
public readonly int _count;

public PersistentTree():this(null){
}

public PersistentTree(IComparer comp){
	this.comp = comp;
	tree = null;
	_count = 0;
}

public int count(){
	return _count;
	}

public int capacity(){
	return _count;
	}
	
public bool contains(Object key){
	return find(key) != null;
}

	public IPersistentMap add(Object key)
		{
	return put(key, null);
}

public IPersistentMap put(Object key, Object val){
	Box found = new Box(null);
	Node t = add(tree, key, val, found);
	if(t == null)   //null == already contains key
		{
		Node foundNode = (Node) found.val;
		if(foundNode.val() == val)  //note only get same collection on identity of val, not equals()
			return this;
		return new PersistentTree(comp, replace(tree, key, val), _count);
		}
	return new PersistentTree(comp, t.blacken(), _count + 1);
}


public IPersistentMap remove(Object key){
	Box found = new Box(null);
	Node t = remove(tree, key, found);
	if(t == null)
		{
		if(found.val == null)//null == doesn't contain key
			return this;
		//empty
		return new PersistentTree(comp);
		}
	return new PersistentTree(comp, t.blacken(), _count - 1);
}

public ISeq seq() {
    if(_count > 0)
        return Seq.create(tree, true);
    return null;
}

public ISeq rseq() {
    if(_count > 0)
        return Seq.create(tree, false);
    return null;
}

public IEnumerator GetEnumerator(){
	return new NodeIEnumerator(tree, true);
}

public NodeIEnumerator reverseIEnumerator(){
	return new NodeIEnumerator(tree, false);
}

public IEnumerator keys(){
return keys((NodeIEnumerator)GetEnumerator());
}

public IEnumerator vals(){
return vals((NodeIEnumerator)GetEnumerator());
}

public IEnumerator keys(NodeIEnumerator it){
	return new KeyIEnumerator(it);
}

public IEnumerator vals(NodeIEnumerator it){
	return new ValIEnumerator(it);
}

public Object minKey(){
	Node t = min();
	return t!=null?t._key:null;
}

public Node min(){
	Node t = tree;
	if(t != null)
		{
		while(t.left() != null)
			t = t.left();
		}
	return t;
}

public Object maxKey(){
	Node t = max();
	return t!=null?t._key:null;
}

public Node max(){
	Node t = tree;
	if(t != null)
		{
		while(t.right() != null)
			t = t.right();
		}
	return t;
}

public int depth(){
	return depth(tree);
}

int depth(Node t){
	if(t == null)
		return 0;
	return 1 + Math.Max(depth(t.left()), depth(t.right()));
}

public Object get(Object key){
	Node n = (Node)find(key);
	return (n != null) ? n.val() : null;
}

public IMapEntry find(Object key){
	Node t = tree;
	while(t != null)
		{
		int c = compare(key, t._key);
		if(c == 0)
			return t;
		else if(c < 0)
			t = t.left();
		else
			t = t.right();
		}
	return t;
}

int compare(Object k1, Object k2){
	if(comp != null)
		return comp.Compare(k1, k2);
	return ((IComparable) k1).CompareTo(k2);
}

Node add(Node t, Object key, Object val, Box found){
	if(t == null)
		{
		if(val == null)
			return new Red(key);
		return new RedVal(key, val);
		}
	int c = compare(key, t._key);
	if(c == 0)
		{
		found.val = t;
		return null;
		}
	Node ins = c < 0 ? add(t.left(), key, val, found) : add(t.right(), key, val, found);
	if(ins == null) //found below
		return null;
	if(c < 0)
		return t.addLeft(ins);
	return t.addRight(ins);
}

Node remove(Node t, Object key, Box found){
	if(t == null)
		return null; //not found indicator
	int c = compare(key, t._key);
	if(c == 0)
		{
		found.val = t;
		return append(t.left(), t.right());
		}
	Node del = c < 0 ? remove(t.left(), key, found) : remove(t.right(), key, found);
	if(del == null && found.val == null) //not found below
		return null;
	if(c < 0)
		{
		if(t.left() is Black)
			return balanceLeftDel(t._key, t.val(), del, t.right());
		else
			return red(t._key, t.val(), del, t.right());
		}
	if(t.right() is Black)
		return balanceRightDel(t._key, t.val(), t.left(), del);
	return red(t._key, t.val(), t.left(), del);
//		return t.removeLeft(del);
//	return t.removeRight(del);
}

static Node append(Node left, Node right){
	if(left == null)
		return right;
	else if(right == null)
		return left;
	else if(left is Red)
		{
		if(right is Red)
			{
			Node app = append(left.right(), right.left());
			if(app is Red)
				return red(app._key, app.val(),
				           red(left._key, left.val(), left.left(), app.left()),
				           red(right._key, right.val(), app.right(), right.right()));
			else
				return red(left._key, left.val(), left.left(), red(right._key, right.val(), app, right.right()));
			}
		else
			return red(left._key, left.val(), left.left(), append(left.right(), right));
		}
	else if(right is Red)
		return red(right._key, right.val(), append(left, right.left()), right.right());
	else //black/black
		{
		Node app = append(left.right(), right.left());
		if(app is Red)
			return red(app._key, app.val(),
			           black(left._key, left.val(), left.left(), app.left()),
			           black(right._key, right.val(), app.right(), right.right()));
		else
			return balanceLeftDel(left._key, left.val(), left.left(), black(right._key, right.val(), app, right.right()));
		}
}

static Node balanceLeftDel(Object key, Object val, Node del, Node right){
	if(del is Red)
		return red(key, val, del.blacken(), right);
	else if(right is Black)
		return rightBalance(key, val, del, right.redden());
	else if(right is Red && right.left() is Black)
		return red(right.left()._key, right.left().val(),
		           black(key, val, del, right.left().left()),
		           rightBalance(right._key, right.val(), right.left().right(), right.right().redden()));
	else
		throw new InvalidOperationException("Invariant violation");
}

static Node balanceRightDel(Object key, Object val, Node left, Node del){
	if(del is Red)
		return red(key, val, left, del.blacken());
	else if(left is Black)
		return leftBalance(key, val, left.redden(), del);
	else if(left is Red && left.right() is Black)
		return red(left.right()._key, left.right().val(),
		           leftBalance(left._key, left.val(), left.left().redden(), left.right().left()),
		           black(key, val, left.right().right(), del));
	else
		throw new InvalidOperationException("Invariant violation");
}

static Node leftBalance(Object key, Object val, Node ins, Node right){
	if(ins is Red && ins.left() is Red)
		return red(ins._key, ins.val(), ins.left().blacken(), black(key, val, ins.right(), right));
	else if(ins is Red && ins.right() is Red)
		return red(ins.right()._key, ins.right().val(),
		           black(ins._key, ins.val(), ins.left(), ins.right().left()),
		           black(key, val, ins.right().right(), right));
	else
		return black(key, val, ins, right);
}


static Node rightBalance(Object key, Object val, Node left, Node ins){
	if(ins is Red && ins.right() is Red)
		return red(ins._key, ins.val(), black(key, val, left, ins.left()), ins.right().blacken());
	else if(ins is Red && ins.left() is Red)
		return red(ins.left()._key, ins.left().val(),
		           black(key, val, left, ins.left().left()),
		           black(ins._key, ins.val(), ins.left().right(), ins.right()));
	else
		return black(key, val, left, ins);
}

Node replace(Node t, Object key, Object val){
	int c = compare(key, t._key);
	return t.replace(t._key,
	                 c == 0 ? val : t.val(),
	                 c < 0 ? replace(t.left(), key, val) : t.left(),
	                 c > 0 ? replace(t.right(), key, val) : t.right());
}

PersistentTree(IComparer comp, Node tree, int count){
	this.comp = comp;
	this.tree = tree;
	this._count = count;
}

static Red red(Object key, Object val, Node left, Node right){
	if(left == null && right == null)
		{
		if(val == null)
			return new Red(key);
		return new RedVal(key, val);
		}
	if(val == null)
		return new RedBranch(key, left, right);
	return new RedBranchVal(key, val, left, right);
}

static Black black(Object key, Object val, Node left, Node right){
	if(left == null && right == null)
		{
		if(val == null)
			return new Black(key);
		return new BlackVal(key, val);
		}
	if(val == null)
		return new BlackBranch(key, left, right);
	return new BlackBranchVal(key, val, left, right);
}


public abstract class Node : IMapEntry{
	internal readonly Object _key;

	internal Node(Object key){
		this._key = key;
	}

	public Object key(){
		return _key;
	}

	virtual public Object val(){
		return null;
	}

    internal virtual Node left()
        {
		return null;
	}

    internal virtual Node right()
        {
		return null;
	}

	internal abstract Node addLeft(Node ins);

    internal abstract Node addRight(Node ins);

    internal abstract Node removeLeft(Node del);

    internal abstract Node removeRight(Node del);

    internal abstract Node blacken();

    internal abstract Node redden();

    internal virtual Node balanceLeft(Node parent)
        {
		return black(parent._key, parent.val(), this, parent.right());
	}

    internal virtual Node balanceRight(Node parent)
        {
		return black(parent._key, parent.val(), parent.left(), this);
	}

    internal abstract Node replace(Object key, Object val, Node left, Node right);
}

class Black : Node{
	public Black(Object key):base(key){
	}

	internal override Node addLeft(Node ins){
		return ins.balanceLeft(this);
	}

    internal override Node addRight(Node ins)
        {
		return ins.balanceRight(this);
	}

    internal override Node removeLeft(Node del)
        {
		return balanceLeftDel(_key, val(), del, right());
	}

    internal override Node removeRight(Node del)
        {
		return balanceRightDel(_key, val(), left(), del);
	}

    internal override Node blacken()
        {
		return this;
	}

    internal override Node redden()
        {
		return new Red(_key);
	}

    internal override Node replace(Object key, Object val, Node left, Node right)
        {
		return black(key, val, left, right);
	}
}

class BlackVal : Black{
	readonly Object _val;

	public BlackVal(Object key, Object val):base(key){
		this._val = val;
	}

	override public Object val(){
		return _val;
	}

    internal override Node redden()
        {
		return new RedVal(_key, _val);
	}

}

class BlackBranch : Black{
	internal readonly Node _left;
	internal readonly Node _right;

	public BlackBranch(Object key, Node left, Node right):base(key){
		this._left = left;
		this._right = right;
	}

    internal override Node left()
        {
		return _left;
	}

    internal override Node right()
        {
		return _right;
	}

    internal override Node redden()
        {
		return new RedBranch(_key, _left, _right);
	}

}

class BlackBranchVal : BlackBranch{
	readonly Object _val;

	public BlackBranchVal(Object key, Object val, Node left, Node right): base(key, left, right){
		this._val = val;
	}

	override public Object val(){
		return _val;
	}

    internal override Node redden()
        {
		return new RedBranchVal(_key, _val, _left, _right);
	}

}

class Red : Node{
	public Red(Object key):base(key){
	}

    internal override Node addLeft(Node ins)
        {
		return red(_key, val(), ins, right());
	}

    internal override Node addRight(Node ins)
        {
		return red(_key, val(), left(), ins);
	}

    internal override Node removeLeft(Node del)
        {
		return red(_key, val(), del, right());
	}

    internal override Node removeRight(Node del)
        {
		return red(_key, val(), left(), del);
	}

    internal override Node blacken()
        {
		return new Black(_key);
	}

    internal override Node redden()
        {
		throw new InvalidOperationException("Invariant violation");
	}

    internal override Node replace(Object key, Object val, Node left, Node right)
        {
		return red(key, val, left, right);
	}
}

class RedVal : Red{
	readonly Object _val;

	public RedVal(Object key, Object val):base(key){
		this._val = val;
	}

	override public Object val(){
		return _val;
	}

    internal override Node blacken()
        {
		return new BlackVal(_key, _val);
	}
}

class RedBranch : Red{
	internal readonly Node _left;
	internal readonly Node _right;

	public RedBranch(Object key, Node left, Node right):base(key){
		this._left = left;
		this._right = right;
	}

    internal override Node left()
        {
		return _left;
	}

    internal override Node right()
        {
		return _right;
	}

    internal override Node balanceLeft(Node parent)
        {
		if(_left is Red)
			return red(_key, val(), _left.blacken(), black(parent._key, parent.val(), _right, parent.right()));
		else if(_right is Red)
			return red(_right._key, _right.val(), black(_key, val(), _left, _right.left()),
			           black(parent._key, parent.val(), _right.right(), parent.right()));
		else
			return base.balanceLeft(parent);

	}

    internal override Node balanceRight(Node parent)
        {
		if(_right is Red)
			return red(_key, val(), black(parent._key, parent.val(), parent.left(), _left), _right.blacken());
		else if(_left is Red)
			return red(_left._key, _left.val(), black(parent._key, parent.val(), parent.left(), _left.left()),
			           black(_key, val(), _left.right(), _right));
		else
			return base.balanceRight(parent);
	}

    internal override Node blacken()
        {
		return new BlackBranch(_key, _left, _right);
	}
}

class RedBranchVal : RedBranch{
	readonly Object _val;

	public RedBranchVal(Object key, Object val, Node left, Node right):base(key, left, right){
		
		this._val = val;
	}

	override public Object val(){
		return _val;
	}

    internal override Node blacken()
        {
		return new BlackBranchVal(_key, _val, _left, _right);
	}
}

public class Seq : ISeq{
	readonly ISeq stack;
	readonly bool asc;

    Seq(ISeq stack, bool asc) {
        this.stack = stack;
        this.asc = asc;
    }

    internal static Seq create(Node t, bool asc){
		return new Seq(push(t, null, asc),asc);
	}

	static ISeq push(Node t, ISeq stack, bool asc){
		while(t != null)
			{
			stack = RT.cons(t,stack);
			t = asc ? t.left() : t.right();
			}
        return stack;
    }

    public Object first() {
        return stack.first();
    }

    public ISeq rest() {
        Node t = (Node)stack.first();
        ISeq nextstack = push(asc ? t.right() : t.left(), stack.rest(), asc);
        if(nextstack != null)
            {
            return new Seq(nextstack,asc);
            }
        return null;
    }
}


public class NodeIEnumerator : IEnumerator{
	Stack stack = new Stack();
	bool asc;
    object curr;

	internal NodeIEnumerator(Node t, bool asc){
		this.asc = asc;
		push(t);
	}

	void push(Node t){
		while(t != null)
			{
			stack.Push(t);
			t = asc ? t.left() : t.right();
			}
	}

	bool hasNext(){
		return !(stack.Count == 0);
	}

	Object next(){
		Node t = (Node) stack.Pop();
		push(asc ? t.right() : t.left());
		return t;
	}

	public void remove(){
		throw new InvalidOperationException();
	}

#region IEnumerator Members

public object Current
    {
    get { return curr; }
    }

public bool MoveNext()
    {
    if (hasNext())
        {
        curr = next();
        return true;
        }
    return false;
    }

public void Reset()
    {
    throw new Exception("The method or operation is not implemented.");
    }

#endregion
    }

class KeyIEnumerator : IEnumerator{
	NodeIEnumerator it;

	internal KeyIEnumerator(NodeIEnumerator it){
		this.it = it;
	}

#region IEnumerator Members

public object  Current
{
get { return ((Node)it.Current)._key; }
}

public bool  MoveNext()
{
return it.MoveNext();
}

public void  Reset()
{
 	throw new Exception("The method or operation is not implemented.");
}

#endregion
}

class ValIEnumerator : IEnumerator{
	NodeIEnumerator it;

	internal ValIEnumerator(NodeIEnumerator it){
		this.it = it;
	}

#region IEnumerator Members

public object Current
    {
    get { return ((Node)it.Current).val(); }
    }

public bool MoveNext()
    {
    return it.MoveNext();
    }

public void Reset()
    {
    throw new Exception("The method or operation is not implemented.");
    }

#endregion
    }

    //*
	[STAThread]
static public void Main(String[] args){
	if(args.Length != 1)
		Console.Error.WriteLine("Usage: PersistentTree n");
	int n = Int32.Parse(args[0]);
	Object[] ints = new Object[n];
	for(int i = 0; i < ints.Length; i++)
		{
		ints[i] = i;
		}
	//Collections.shuffle(Arrays.asList(ints));
    Array.Reverse(ints);
	Console.WriteLine("Building set");
	//IPersistentMap set = new PersistentTree();
	//IPersistentMap set = new PersistentArrayMap();
	//IPersistentMap set = new PersistentListMap();
	//IPersistentMap set = new PersistentHashtableMap(1001);
	IPersistentMap set = new PersistentHybridMap(1001);
    //for(int i = 0; i < ints.Length; i++)
    //    {
    //    Object anInt = ints[i];
    //    set = set.add(anInt);
    //    }
	DateTime start = DateTime.Now;
	for (int i = 0; i < ints.Length; i++)
		{
		Object anInt = ints[i];
		set = set.put(anInt, anInt);
		}

	foreach(IMapEntry e in set)
		{
		if(!set.contains(e.key()))
			Console.Error.WriteLine("Can't find: " + e.key());
		//else if(n < 2000)
		//	Console.Write(e.key().ToString() + ",");
		}

	for(int i = 0; i < ints.Length/2; i++)
		{
		Object anInt = ints[i];
		set = set.remove(anInt);
		}

	Console.WriteLine("Time: " + (DateTime.Now - start));
	Console.Error.WriteLine("count = " + set.count());

	Console.WriteLine("Building Hashtable");
	Hashtable od = Hashtable.Synchronized(new Hashtable(1001));
	start = DateTime.Now;
	for (int i = 0; i < ints.Length; i++)
		{
		Object anInt = ints[i];
		od.Add(anInt, anInt);
		}

	foreach (DictionaryEntry e in od)
		{
		if (!od.Contains(e.Key))
			Console.Error.WriteLine("Can't find: " + e.Key);
		//else if (n < 2000)
		//	Console.Write(e.key().ToString() + ",");
		}

	for (int i = 0; i < ints.Length / 2; i++)
		{
		Object anInt = ints[i];
		od.Remove(anInt);
		}

	Console.WriteLine("Time: " + (DateTime.Now - start));
	Console.Error.WriteLine("count = " + od.Count);

}
     //*/
}
    }
