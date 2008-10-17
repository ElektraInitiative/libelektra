#include <Python.h>
#include <string.h>
#include <stdlib.h>
#include <kdb.h>
//#include <kdbprivate.h>

static PyObject* py_kdbOpen (PyObject *self, PyObject *args)
{
	KDB *handle;
	PyObject *LongPtr;
	if (!PyArg_Parse(args, ""))
		return NULL;
	else {
		handle=kdbOpen();
		LongPtr=PyLong_FromVoidPtr(handle);
		return LongPtr;
	}

}

static PyObject* py_kdbGetChildKeys(PyObject *self, PyObject *args)
{
	PyObject *py_ks;
	PyObject *py_h;
	char *s;
	long opt;
	unsigned long options;
	ssize_t ret;
	int r;
	KDB *h;
	KeySet *ks;

	if (!PyArg_Parse(args,"(OsOl)",&py_h,&s,&py_ks,&opt))
		return NULL;
	else {
		options=(unsigned long) opt;
		h=PyLong_AsVoidPtr(py_h);
		ks=PyLong_AsVoidPtr(py_ks);
		
		ret=kdbGetChildKeys(h,s,ks,options);
		r=(int) ret;
		return Py_BuildValue("i",r);
	}
}

static PyObject* py_kdbGetKeyChildKeys(PyObject *self, PyObject *args)
{
	PyObject *py_ks;
	PyObject *py_k;
	PyObject *py_h;
	long opt;
	unsigned long options;
	ssize_t ret;
	int r;
	KDB *h;
	KeySet *ks;
	Key *k;

	if (!PyArg_Parse(args,"(OOOl)",&py_h,&py_k,&py_ks,&opt))
		return NULL;
	else {
		options=(unsigned long) opt;
		h=PyLong_AsVoidPtr(py_h);
		k=PyLong_AsVoidPtr(py_k);
		ks=PyLong_AsVoidPtr(py_ks);
		
		ret=kdbGetKeyChildKeys(h,k,ks,options);
		r=(int) ret;
		return Py_BuildValue("i",r);
	}
}

static PyObject* py_ksNew(PyObject *self, PyObject *args) {
	KeySet *ks;
	PyObject* LongPtr;
	if (!PyArg_Parse(args, ""))
		return NULL;
	else {
		ks=ksNew();
		LongPtr=PyLong_FromVoidPtr(ks);
		return LongPtr;
	}
}

static PyObject* py_ksLookupByName(PyObject *self, PyObject *args) {
	KeySet *ks;
	PyObject *py_ks, *LongPtr;
	char *s;
	long l;
	unsigned long ul;
	Key *ret;
	if (!PyArg_Parse(args, "(Osi)",&py_ks,&s,&l))
		return NULL;
	else {
		ul=(unsigned long) l;
		ks=PyLong_AsVoidPtr(py_ks);
		ret=ksLookupByName(ks,s,ul);
		LongPtr=PyLong_FromVoidPtr(ret);
		//return Py_BuildValue("O", LongPtr);
		return LongPtr;
	}
}

/*
static PyObject* py_keyStealValue(PyObject *self, PyObject *args) {
	Key *k;
	PyObject *py_k;
	char *s;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else {
		k=PyLong_AsVoidPtr(py_k);
		s=keyStealValue(k);
		return Py_BuildValue("s", s);
	}
}
*/

static PyObject* py_kdbClose(PyObject *self, PyObject *args)
{
	PyObject *py_h;
	KDB *h;
	if (!PyArg_Parse(args, "(O)",&py_h))
		return NULL;
	else {
		h=PyLong_AsVoidPtr(py_h);
		return Py_BuildValue("i", kdbClose(h));
	}
}

static PyObject* py_ksDel(PyObject *self, PyObject *args)
{
	PyObject *py_ks;
	KeySet *ks;
	if (!PyArg_Parse(args, "(O)",&py_ks))
		return NULL;
	else {
		ks=PyLong_AsVoidPtr(py_ks);
		return Py_BuildValue("i", ksDel(ks));
	}
}

/* keyNew is only implement without parameters. Options can be set with
 * the correspondending functions */
static PyObject* py_keyNew(PyObject *self, PyObject *args)
{
	PyObject *LongPtr;
	Key *k;
	char *s;
	if (!PyArg_Parse(args, "(s)",&s))
		return NULL;
	else  {
		k=keyNew(s,KEY_SWITCH_END);
		LongPtr=PyLong_FromVoidPtr(k);
		//return Py_BuildValue("O",LongPtr);
		return LongPtr;
	}
}

static PyObject* py_keySetName(PyObject *self, PyObject *args)
{
	PyObject *py_k;
	Key *k;
	char *s;
	ssize_t ret;
	int r;
	if (!PyArg_Parse(args, "(Os)",&py_k,&s))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret=keySetName(k,s);
		r=(int) ret;
		return Py_BuildValue("i",r);
	}
}

static PyObject* py_keySetBinary(PyObject *self, PyObject *args)
{
	PyObject *py_k;
	Key *k;
	char *s;
	ssize_t ret;
	int r;
	int i;
	if (!PyArg_Parse(args, "(Os#)",&py_k,&s,&i))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret=keySetBinary(k,s,(size_t)i);
		r=(int) ret;
		return Py_BuildValue("i",r);
	}
}

static PyObject* py_kdbSetKey(PyObject *self, PyObject *args)
{
	PyObject *py_k;
	PyObject *py_h;
	Key *k;
	KDB *h;
	int ret;
	if (!PyArg_Parse(args, "(OO)",&py_h,&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		h=PyLong_AsVoidPtr(py_h);
		ret=kdbSetKey(h,k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_ksRewind(PyObject *self, PyObject *args)
{
	PyObject *py_ks;
	KeySet *ks;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_ks))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		ret=ksRewind(ks);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_ksNext(PyObject *self, PyObject *args)
{
	PyObject *py_ks;
	KeySet *ks;
	Key *k;
	PyObject *py_k;
	if (!PyArg_Parse(args, "(O)",&py_ks))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		k=ksNext(ks);
		py_k=PyLong_FromVoidPtr(k);
		//return Py_BuildValue("O",py_k);
		return py_k;
	}
}

static PyObject* py_ksGetSize(PyObject *self, PyObject *args)
{
	PyObject *py_ks;
	KeySet *ks;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_ks))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		ret=(int)ksGetSize(ks);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetFullName(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char result[1024];
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		(void) keyGetFullName(k,result,1023); // TODO return value not used
		return Py_BuildValue("s",result);
	}
	
}

static PyObject* py_keyGetString(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char result[1024];
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		(void) keyGetString(k,result,1023); // TODO return value not used
		return Py_BuildValue("s",result);
	}
	
}

static PyObject* py_keySetString(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char *s;
	int ret;
	if (!PyArg_Parse(args, "(Os)",&py_k,&s))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret=(int)keySetString(k,s);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_kdbSetKeys(PyObject *self, PyObject *args)
{
	KeySet *ks;
	KDB *h;
	PyObject *py_ks;
	PyObject *py_h;
	int ret;
	if (!PyArg_Parse(args, "(OO)",&py_h,&py_ks))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		h=PyLong_AsVoidPtr(py_h);
		ret=kdbSetKeys(h,ks);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetNameSize(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret=(int)keyGetNameSize(k);
		return Py_BuildValue("i",ret);
	}
}

/*
static PyObject* py_keyGetDataSize(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret=(int)keyGetDataSize(k);
		return Py_BuildValue("i",ret);
	}
}*/

static PyObject* py_keyGetCommentSize(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	ssize_t ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret=keyGetCommentSize(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetName(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char result[1024];
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		(void) keyGetName(k,result,1023); // TODO return value not used
		return Py_BuildValue("s",result);
	}
	
}

static PyObject* py_keyGetComment(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char result[1024];
	result[0]='\0';
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		(void) keyGetComment(k,result,1023); // TODO return value not used
		return Py_BuildValue("s",result);
	}
	
}

static PyObject* py_kdbGetValue(PyObject *self, PyObject *args)
{
	KDB *h;
	PyObject *py_h;
	char result[1024];
	char *s;
	if (!PyArg_Parse(args, "(Os)",&py_h,&s))
		return NULL;
	else  {
		h=PyLong_AsVoidPtr(py_h);
		(void) kdbGetValue(h,s,result,1023); // TODO return value not used
		return Py_BuildValue("s",result);
	}
}

static PyObject* py_kdbGetKeyByParent(PyObject *self, PyObject *args)
{
	KDB *h;
	PyObject *py_h;
	Key *k;
	PyObject *py_k;
	char result[1024];
	char *s1;
	char *s2;
	int ret;
	if (!PyArg_Parse(args, "(OssO)",&py_h,&s1,&s2,&py_k))
		return NULL;
	else  {
		h=PyLong_AsVoidPtr(py_h);
		k=PyLong_AsVoidPtr(py_k);
		ret = kdbGetKeyByParent(h,s1,s2,k); 
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_kdbGetKeyByParentKey(PyObject *self, PyObject *args)
{
	KDB *h;
	PyObject *py_h;
	Key *k1;
	PyObject *py_k1;
	Key *k2;
	PyObject *py_k2;
	char *s;
	int ret;
	if (!PyArg_Parse(args, "(OssO)",&py_h,&py_k1,&s,&py_k2))
		return NULL;
	else  {
		h=PyLong_AsVoidPtr(py_h);
		k1=PyLong_AsVoidPtr(py_k1);
		k2=PyLong_AsVoidPtr(py_k2);
		ret = kdbGetKeyByParentKey(h,k1,s,k2);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_kdbGetValueByParent(PyObject *self, PyObject *args)
{
	KDB *h;
	PyObject *py_h;
	char result[1024];
	char *s1;
	char *s2;
	if (!PyArg_Parse(args, "(Oss)",&py_h,&s1,&s2))
		return NULL;
	else  {
		h=PyLong_AsVoidPtr(py_h);
		(void) kdbGetValueByParent(h,s1,s2,result,1023);  // TODO return value not used
		return Py_BuildValue("s",result);
	}
}

static PyObject* py_kdbSetValue(PyObject *self, PyObject *args)
{
	KDB *h;
	PyObject *py_h;
	char *s1;
	char *s2;
	int ret;
	if (!PyArg_Parse(args, "(Oss)",&py_h,&s1,&s2))
		return NULL;
	else  {
		h=PyLong_AsVoidPtr(py_h);
		ret = kdbSetValue(h,s1,s2);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_kdbSetValueByParent(PyObject *self, PyObject *args)
{
	KDB *h;
	PyObject *py_h;
	char *s1;
	char *s2;
	char *s3;
	int ret;
	if (!PyArg_Parse(args, "(Osss)",&py_h,&s1,&s2,&s3))
		return NULL;
	else  {
		h=PyLong_AsVoidPtr(py_h);
		ret = kdbSetValueByParent(h,s1,s2,s3);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_kdbGetKey(PyObject *self, PyObject *args)
{
	KDB *h;
	PyObject *py_h;
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(OO)",&py_h,&py_k))
		return NULL;
	else  {
		h=PyLong_AsVoidPtr(py_h);
		k=PyLong_AsVoidPtr(py_k);
		ret = kdbGetKey(h,k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_kdbStrErr(PyObject *self, PyObject *args)
{
	int no;
	char *s;
	if (!PyArg_Parse(args, "(i)",&no))
		return NULL;
	else  {
		s = kdbStrError(no);
		return Py_BuildValue("s",s);
	}
}

static PyObject* py_kdbGetErrno(PyObject *self, PyObject *args)
{
	int no;
	KDB *h;
	PyObject *py_h;
	if (!PyArg_Parse(args, "(O)",&py_h))
		return NULL;
	else  {
		h=PyLong_AsVoidPtr(py_h);
		no = kdbGetErrno();
		return Py_BuildValue("i",no);
	}
}

static PyObject* py_keyInit(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyInit(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyClose(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyClose(k);
		return Py_BuildValue("i",ret);
	}
}

/*
static PyObject* py_keyIsInitialized(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyIsInitialized(k);
		return Py_BuildValue("i",ret);
	}
}*/

static PyObject* py_keyNeedSync(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyNeedSync(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyDup(PyObject *self, PyObject *args)
{
	Key *k1;
	Key *k2;
	PyObject *py_k1;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k1))
		return NULL;
	else  {
		k1=PyLong_AsVoidPtr(py_k1);
		k2 = keyDup(k1);
		return PyLong_FromVoidPtr(k2);
	}
}

static PyObject* py_keyGetType(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyGetType(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keySetType(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int type;
	int ret;
	if (!PyArg_Parse(args, "(Oi)",&py_k,&type))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keySetType(k,type);
		return Py_BuildValue("i",ret);
	}
}

/* TODO
static PyObject* py_keySetDir(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyNeedSync(k);
		return Py_BuildValue("i",ret);
	}
}*/

/*
static PyObject* py_keySetFlag(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keySetFlag(k);
		return Py_BuildValue("i",ret);
	}
}*/

/*
static PyObject* py_keyClearFlag(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyClearFlag(k);
		return Py_BuildValue("i",ret);
	}
}*/

/*
static PyObject* py_keyGetFlag(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyGetFlag(k);
		return Py_BuildValue("i",ret);
	}
}*/

static PyObject* py_keyGetRecordSize(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyGetRecordSize(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetFullNameSize(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyGetFullNameSize(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyName(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	const char *ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyName(k);
		return Py_BuildValue("s",ret);
	}
}

static PyObject* py_keyGetRootName(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char result[1024];
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		(void) keyGetRootName(k,result,1023); // TODO return value not used
		return Py_BuildValue("s",result);
	}
	
}

static PyObject* py_keyGetFullRootName(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char result[1024];
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		(void) keyGetFullRootName(k,result,1023); // TODO return value not used
		return Py_BuildValue("s",result);
	}
	
}

static PyObject* py_keyBaseName(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	const char *s;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		s = keyBaseName(k);
		return Py_BuildValue("s",s);
	}
}

static PyObject* py_keyNameGetBaseNameSize(PyObject *self, PyObject *args)
{
	char *s;
	int size;
	if (!PyArg_Parse(args, "(s)",&s))
		return NULL;
	else  {
		size = keyNameGetBaseNameSize(s);
		return Py_BuildValue("i",size);
	}
}

static PyObject* py_keyGetBaseNameSize(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int size;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		size = keyGetBaseNameSize(k);
		return Py_BuildValue("i",size);
	}
}

static PyObject* py_keyAddBaseName(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char *s;
	int size;
	if (!PyArg_Parse(args, "(Os)",&py_k,&s))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		size = keyAddBaseName(k,s);
		return Py_BuildValue("i",size);
	}
}

static PyObject* py_keySetBaseName(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char *s;
	int size;
	if (!PyArg_Parse(args, "(Os)",&py_k,&s))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		size = keySetBaseName(k,s);
		return Py_BuildValue("i",size);
	}
}

/* TODO
static PyObject* py_keyNameGetOneLevel(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char *s;
	char *name;
	int size;
	if (!PyArg_Parse(args, "(si)",&name,&size))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		s = keyNameGetOneLevel(name,&size);
		return Py_BuildValue("s",s);
	}
} */
/*
static PyObject* py_keyGetParentName(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char result[1024];
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		(void) keyNameGetParentName(k,result,1023); // TODO return value not used
		return Py_BuildValue("s",result);
	}
} 

static PyObject* py_keyGetParentNameSize(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int size;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		size = keyGetParentNameSize(k);
		return Py_BuildValue("i",size);
	}
} */

static PyObject* py_keyNameGetRootNameSize(PyObject *self, PyObject *args)
{
	char *s;
	int size;
	if (!PyArg_Parse(args, "(s)",&s))
		return NULL;
	else  {
		size = keyNameGetRootNameSize(s);
		return Py_BuildValue("i",size);
	}
}

static PyObject* py_keyGetRootNameSize(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int size;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		size = keyGetRootNameSize(k);
		return Py_BuildValue("i",size);
	}
}

static PyObject* py_keyComment(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	const char *s;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		s = keyComment(k);
		return Py_BuildValue("s",s);
	}
}

static PyObject* py_keySetComment(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int size;
	char *s;
	if (!PyArg_Parse(args, "(Os)",&py_k,&s))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		size = keySetComment(k,s);
		return Py_BuildValue("i",size);
	}
}

static PyObject* py_keyGetUID(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int size;
	char *s;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		size = keyGetUID(k);
		return Py_BuildValue("i",size);
	}
}

static PyObject* py_keySetUID(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	char *s;
	int uid;
	if (!PyArg_Parse(args, "(Oi)",&py_k,&uid))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keySetUID(k,uid);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetGID(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int size;
	char *s;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		size = keyGetGID(k);
		return Py_BuildValue("i",size);
	}
}

static PyObject* py_keySetGID(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	int gid;
	if (!PyArg_Parse(args, "(Oi)",&py_k,&gid))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keySetGID(k,gid);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetMode(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int mode;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		mode = keyGetMode(k);
		return Py_BuildValue("i",mode);
	}
}

static PyObject* py_keySetMode(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	int mode;
	if (!PyArg_Parse(args, "(Oi)",&py_k,&mode))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keySetMode(k,mode);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetOwnerSize(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyGetOwnerSize(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyOwner(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	char *s;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		s = keyOwner(k);
		return Py_BuildValue("s",s);
	}
}

static PyObject* py_keySetOwner(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	char *s;
	if (!PyArg_Parse(args, "(Os)",&py_k,&s))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keySetOwner(k,s);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetValueSize(PyObject *self, PyObject *args) 
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyGetValueSize(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetLink(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int mode;
	char result[1024];
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		(void) keyGetLink(k,result,1023); // TODO return value not used
		return Py_BuildValue("s",result);
	}
}

static PyObject* py_keySetLink(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	char *s;
	if (!PyArg_Parse(args, "(Os)",&py_k,&s))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keySetLink(k,s);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetMTime(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyGetMTime(k);
		return Py_BuildValue("l",ret);
	}
}

static PyObject* py_keyGetATime(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyGetATime(k);
		return Py_BuildValue("l",ret);
	}
}

static PyObject* py_keyGetCTime(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyGetCTime(k);
		return Py_BuildValue("l",ret);
	}
}

static PyObject* py_keyIsSystem(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyIsSystem(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyIsUser(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyIsUser(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyGetNamespace(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyGetNamespace(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyIsDir(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyIsDir(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyIsLink(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyIsLink(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyIsBin(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyIsBin(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_keyIsString(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_k))
		return NULL;
	else  {
		k=PyLong_AsVoidPtr(py_k);
		ret = keyIsString(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_ksInsert(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	KeySet *ks;
	PyObject *py_ks;
	int ret;
	if (!PyArg_Parse(args, "(OO)",&py_ks,&py_k))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		k=PyLong_AsVoidPtr(py_k);
		ret = ksInsert(ks,k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_ksAppendKey(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	KeySet *ks;
	PyObject *py_ks;
	int ret;
	if (!PyArg_Parse(args, "(OO)",&py_ks,&py_k))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		k=PyLong_AsVoidPtr(py_k);
		ret = ksAppendKey(ks,k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_ksPop(PyObject *self, PyObject *args)
{
	Key *k;
	PyObject *py_k;
	KeySet *ks;
	PyObject *py_ks;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_ks))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		k = ksPop(ks);
		py_k=PyLong_FromVoidPtr(k);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_ksInsertKeys(PyObject *self, PyObject *args)
{
	KeySet *ks1;
	PyObject *py_ks1;
	KeySet *ks2;
	PyObject *py_ks2;
	int ret;
	if (!PyArg_Parse(args, "(OO)",&py_ks1,&py_ks2))
		return NULL;
	else  {
		ks1=PyLong_AsVoidPtr(py_ks1);
		ks2=PyLong_AsVoidPtr(py_ks2);
		ret = ksInsertKeys(ks1,ks2);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_ksAppend(PyObject *self, PyObject *args)
{
	KeySet *ks1;
	PyObject *py_ks1;
	KeySet *ks2;
	PyObject *py_ks2;
	int ret;
	if (!PyArg_Parse(args, "(OO)",&py_ks1,&py_ks2))
		return NULL;
	else  {
		ks1=PyLong_AsVoidPtr(py_ks1);
		ks2=PyLong_AsVoidPtr(py_ks2);
		ret = ksInsertKeys(ks1,ks2);
		return Py_BuildValue("i",ret);
	}
}

static PyObject* py_ksSort(PyObject *self, PyObject *args)
{
	KeySet *ks;
	PyObject *py_ks;
	int ret;
	if (!PyArg_Parse(args, "(O)",&py_ks))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		ksSort(ks);
		return Py_BuildValue("");
	}
}

static PyObject* py_ksLookupByString(PyObject *self, PyObject *args) 
{
	KeySet *ks;
	PyObject *py_ks, *LongPtr;
	char *s;
	long l;
	unsigned long ul;
	Key *ret;
	if (!PyArg_Parse(args, "(Osi)",&py_ks,&s,&l))
		return NULL;
	else {
		ul=(unsigned long) l;
		ks=PyLong_AsVoidPtr(py_ks);
		ret=ksLookupByString(ks,s,ul);
		LongPtr=PyLong_FromVoidPtr(ret);
		return LongPtr;
	}
}

static PyObject* py_ksLookupByBinary(PyObject *self, PyObject *args) 
{
	KeySet *ks;
	PyObject *py_ks, *LongPtr;
	char *s;
	int len;
	long l;
	unsigned long ul;
	Key *ret;
	if (!PyArg_Parse(args, "(Os#i)",&py_ks,&s,&len,&l))
		return NULL;
	else {
		ul=(unsigned long) l;
		ks=PyLong_AsVoidPtr(py_ks);
		ret=ksLookupByBinary(ks,s,len,ul);
		LongPtr=PyLong_FromVoidPtr(ret);
		return LongPtr;
	}
}

static PyObject* py_ksCurrent(PyObject *self, PyObject *args)
{
	PyObject *py_ks;
	KeySet *ks;
	Key *k;
	PyObject *py_k;
	if (!PyArg_Parse(args, "(O)",&py_ks))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		k=ksCurrent(ks);
		py_k=PyLong_FromVoidPtr(k);
		return py_k;
	}
}

static PyObject* py_ksHead(PyObject *self, PyObject *args)
{
	PyObject *py_ks;
	KeySet *ks;
	Key *k;
	PyObject *py_k;
	if (!PyArg_Parse(args, "(O)",&py_ks))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		k=ksHead(ks);
		py_k=PyLong_FromVoidPtr(k);
		return py_k;
	}
}

static PyObject* py_ksTail(PyObject *self, PyObject *args)
{
	PyObject *py_ks;
	KeySet *ks;
	Key *k;
	PyObject *py_k;
	if (!PyArg_Parse(args, "(O)",&py_ks))
		return NULL;
	else  {
		ks=PyLong_AsVoidPtr(py_ks);
		k=ksTail(ks);
		py_k=PyLong_FromVoidPtr(k);
		return py_k;
	}
}

static PyObject* py_kdbRemove(PyObject *self, PyObject *args)
{
	PyObject *py_kdb;
	KDB *kdb;
	char *s;
	int ret;
	if (!PyArg_Parse(args, "(Os)",&py_kdb,&s))
		return NULL;
	else  {
		kdb=PyLong_AsVoidPtr(py_kdb);
		ret=kdbRemove(kdb,s);
		return Py_BuildValue("i",ret);
	}
}

static struct PyMethodDef elektra_methods[] = {
	// Tested
	{"kdbGetChildKeys",py_kdbGetChildKeys,1,"kdbGetChildKeys(handle,parentName,keySet,options) is similar and calls kdbGetKeyChildKeys().\n"
						"It is provided for convenience."
						"Instead of passing the parentName with a key it directly uses a string.\n"
						"Parameters: \n"
						"\thandle\t key Database handle\n"
						"\tparentName\t Name of the parent key\n"
						"\tkeySet\t key Set handle, the result is stored in this ks\n"
						"\toptions\t options bits\n"
						"Return: handle to the key, 0 otherwise"},
	{"kdbGetKeyChildKeys",py_kdbGetKeyChildKeys,1,	"kdbGetKeyChildKeys(handle,parentKey,ks,options) Retrieve a number of inter-related keys\n"
							"at once. This is one of the most practical methods of the library, and you should use\n"
							"it to retrieve in one shot all keys needed by your application.\n"
							"Parameters: \n"
							"\thandle\t key Database handle\n"
							"\tparentName\t Name of the parent key\n"
							"\tkeySet\t key Set handle, the result is stored in this ks\n"
							"\toptions\t options bits\n"
							"Return: handle to the key, 0 otherwise"},
	{"kdbOpen",py_kdbOpen,0,"kdbOpen() opens the session to the key database and returns a handle."},
	{"ksNew",py_ksNew,0,"ksNew() creates a new keyset."},
	{"ksDel",py_ksDel,1,"ksDel(ks) deletes a keyset."},
	{"kdbClose",py_kdbClose,1,"kdbClose() closes the session to the key database"},
	{"ksLookupByName",py_ksLookupByName,1,
		"ksLookupByName(ks,name,options)\nLook for a Key contained in ks that matches name, starting from ks' ksNext() position.\n"
		"Parameters: \n"
		"\tks\t where to look for\n"
		"\tname\t key name you are looking for\n"
		"\toptions\t options bits\n"
		"Return: handle to the key, 0 otherwise"
		},
//	{"keyStealValue",py_keyStealValue,1},
	{"keyNew",py_keyNew,1,"keyNew(name) creates a new key with name <name>. Returns a key handle."},
	{"keySetName",py_keySetName,0,	"keySetName(key,name) sets a new name to the key.\n"
					"Parameters: \n"
					"\tkey\t the key handle\n"
					"\tname\t new key name\n"
					"Return: size in bytes of this new key"
					},
	{"keySetBinary",py_keySetBinary,1,	"keySetBinary(key,value) sets the binary value of a key.\n"
					"Parameters: \n"
					"\tkey\t the key handle\n"
					"\tvalue\t new key value\n"
					"Return: size in bytes of this new key"
					},
	{"kdbSetKey",py_kdbSetKey,1,"kdbSetKey(handle,key) sets key in the backend storage.\n"
					"Parameters: \n"
					"\thandle\t kdb handle\n"
					"\tkey\t key to set\n"
					"Return: 0 on succes, -1 on failure"
					},
	{"ksRewind",py_ksRewind,1,	"ksRewind(ks) Resets a keySet internal cursor.\n"
					"Parameter:\n"
					"\tks\tKeySet\n"
					"Returns: always 0"},
	{"ksNext",py_ksNext,1,	"ksNext(ks) returns the next Key in a KeySet.\n"
					"Parameter:\n"
					"\tks\tKeySet\n"
					"Returns: the new current key"},
	{"keyGetFullName",py_keyGetFullName,1,	"keyGetFullName(key) returns the full name of the key.\n"
					"Parameter:\n"
					"\tkey\tthe key, which name should be requested\n"
					"Returns: full name of the key"},
	{"ksGetSize",py_ksGetSize,1,	"ksGetSize(ks) Return the number of keys that ks contains.\n"
					"Parameter:\n"
					"\tks\tKeySet\n"
					"Returns: the number of keys that ks contains."},
	{"keyGetString",py_keyGetString,1,	"keyGetString(key) returns the string value of the key.\n"
						"Parameter:\n"
						"\tkey\tthe key\n"
						"Returns: the string value of the key"},
	{"keySetString",py_keySetString,1,"keySetString(key,string) sets the string value of the key.\n"
						"Parameter:\n"
						"\tkey\tthe key\n"
						"\tstring\tthe string to be set\n"
						"Returns: number of characters of the string"},
	{"kdbSetKeys",py_kdbSetKeys,1,	"kdbSetKeys(handle,ks) commits the ks KeySet to the backend storage, starting from ks's current "
					"position until its end.\n"
					"Parameters: \n"
					"\thandle\t kdb handle\n"
					"\tks\t keySet handle\n"
					"Return: 0 on succes, -1 on failure"
					},

	{"keyGetNameSize",py_keyGetNameSize,1,"keyGetNameSize(key) returns the length of the key name"},
	{"keyGetCommentSize",py_keyGetCommentSize,1,"keyGetCommentSize(key) returns the length of the comment"},
	{"keyGetName",py_keyGetName,1,"keyGetName(key) returns the key name."},
	{"keyGetComment",py_keyGetComment,1,"keyGetComment(key) returns the key comment."},
	{"keyClose",py_keyClose,1,"keyClose free the key."}, 

	// Not tested

	// The following commented functions are not essential
	/*{"kdbGetValue",py_kdbGetValue,1,"kdbGetValue help message"},
	//{"keyGetDataSize",py_keyGetDataSize,1,"keyGetDataSize(key) returns the length of the key data"},
	{"kdbGetKeyByParent",py_kdbGetKeyByParent,1,"kdbGetKeyByParent help message"},
	{"kdbGetKeyByParentKey",py_kdbGetKeyByParentKey,1,"kdbGetKeyByParentKey help message"},
	{"kdbGetValueByParent",py_kdbGetValueByParent,1,"kdbGetValueByParent help message"},
	{"kdbSetValue",py_kdbSetValue,1,"kdbSetValue help message"},
	{"kdbSetValueByParent",py_kdbSetValueByParent,1,"kdbSetValueByParent help message"}, 
	{"keyInit",py_keyInit,1,"keyInit help message"},
	{"keyIsInitialized",py_keyIsInitialized,1," help message"},
	{"keyNeedSync",py_keyNeedSync,1,"keyNeedSync help message"},
	{"keySetFlag",py_keySetFlag,1,"keySetFlag help message"},
	{"keyClearFlag",py_keyClearFlag,1,"keyClearFlag help message"},
	{"keyGetFlag",py_keyGetFlag,1,"keyGetFlag help message"},
	{"keyGetRootName",py_keyGetRootName,1,"keyGetRootName help message"},
	{"keyGetFullRootName",py_keyGetFullRootName,1,"keyGetFullRootName help message"},
	{"keyNameGetRootNameSize",py_keyNameGetRootNameSize,1,"keyNameGetRootNameSize help message"},
	{"keyGetRootNameSize",py_keyNameGetRootNameSize,1,"keyNameGetRootNameSize help message"},
	{"keyBaseName",py_keyBaseName,1,"keyBaseName help message"},
	{"keyNameGetBaseNameSize",py_keyNameGetBaseNameSize,1,"keyNameGetBaseNameSize help message"},
	{"keyGetBaseNameSize",py_keyGetBaseNameSize,1,"keyGetBaseNameSize help message"},
	{"keyAddBaseName",py_keyAddBaseName,1,"keyAddBaseName help message"},
	{"keySetBaseName",py_keySetBaseName,1,"keySetBaseName help message"},
	*/

	{"kdbGetKey",py_kdbGetKey,1,	"kdbGetKey(handle,key) Fully retrieves the passed key from the backend storage.\n"
					"The backend will try to get the key, identified through its name.\n"
					"Parameters: \n"
					"\thandle\t kdb handle\n"
					"\tkey\t a key that has the name set\n"
					"Return: 0 on succes, -1 on failure"},

	{"kdbStrError",py_kdbStrErr,1,"kdbStrError returns the string for the errno"},
	{"kdbGetErrno",py_kdbGetErrno,1,"kdbGetErrno returns the last errno"},
	{"kdbRemove",py_kdbRemove,1,"kdbRemove(kdb_handle,name) removes a key from the database"},


	{"keyDup",py_keyDup,1,"keyDup(src,dest) creates a duplicate of src in dest. src and dest are key handles."},
	{"keyGetType",py_keyGetType,1,"keyGetType(key) -> returns the type of the key"},
	{"keySetType",py_keySetType,1,"keySetType(key,type) sets the key type to type."},
	//{"keySetDir",py_keySetDir,1,"keySetDir help message"},

	{"keyGetRecordSize",py_keyGetRecordSize,1,"keyGetRecordSize(key) -> int \nreturns the record size of the key."},
	{"keyGetFullNameSize",py_keyGetFullNameSize,1,"keyGetFullNameSize(key) -> int\nreturns the fullname size of the key "},

	{"keyName",py_keyName,1,"keyName(key) -> String\nreturns the name of the key."},

	//{"keyNameGetOneLevel",py_keyNameGetOneLevel,1,"keyNameGetOneLevel help message"}, // TODO
	//{"keyGetParentName",py_keyGetParentName,1,"keyGetParentName help message"},
	//{"keyGetParentNameSize",py_keyGetParentNameSize,1,"keyGetParentNameSize help message"},

	{"keyComment",py_keyComment,1,"keyComment(key) -> String returns the comment of key."},
	{"keySetComment",py_keySetComment,1,"keySetComment(key,value) sets the comment with value."},

	{"keyGetUID",py_keyGetUID,1,"keyGetUID(key) -> int\nreturns the UID."},
	{"keySetUID",py_keySetUID,1,"keySetUID(key,uid) \nsets the UID."},
	{"keyGetGID",py_keyGetGID,1,"keyGetGID(key) -> int\nreturns the GID."},
	{"keySetGID",py_keySetGID,1,"keySetGID(key,gid) sets the gid."},

	{"keyGetMode",py_keyGetMode,1,"keyGetMode(key) -> int returns the mode for the key."},
	{"keySetMode",py_keySetMode,1,"keySetMode(key,int) sets mode."},

	{"keyGetOwnerSize",py_keyGetOwnerSize,1,"keyGetOwnerSize(key) -> int\n returns the size of the owner."},
	{"keyOwner",py_keyOwner,1,"keyOwner(key) -> string\nreturns the owner of the key."},
	{"keySetOwner",py_keySetOwner,1,"keySetOwner(key,owner) sets the key owner to owner."},

	// The following methods are not supported in python binding
	// {"keyGetValueSize",py_keyGetValueSize,1,"keyGetValueSize(key) returns the size of the value."},
	// {"keyValue",py_keyValue,1,"keyValue help message"},
	// {"keyGetBinary",py_keyGetBinary,1,"keyGetBinary help message"},

	{"keyGetLink",py_keyGetLink,1,"keyGetLink(key) -> string\nreturns the link"},
	{"keySetLink",py_keySetLink,1,"keySetLink(key,value)\n sets the link value"},

	{"keyGetMTime",py_keyGetMTime,1,"keyGetMTime(key) -> int\nreturns the MTime."},
	{"keyGetATime",py_keyGetATime,1,"keyGetATime(key) -> int\nreturns the ATime."},
	{"keyGetCTime",py_keyGetCTime,1,"keyGetCTime(key) -> int\nreturns the CTime."},

	{"keyIsSystem",py_keyIsSystem,1,"keyIsSystem(key) -> int\nCheck whether a key is under the system namespace or not.\n"
					"Returns: 1 if key name begins with system, 0 otherwise."
					},
	{"keyIsUser",py_keyIsUser,1,"keyIsUser(key) -> int\nCheck whether a key is under the user namespace or not.\n"
				    "Returns: 1 if key name begins with user, 0 otherwise."
				    },
	//{"keyGetNamespace",py_keyGetNamespace,1,"keyGetNamespace help message"}, //redundant
	{"keyIsDir",py_keyIsDir,1,	"keyIsDir(key) -> int\nCheck if a key is folder key.\n"
					"Returns: 1 if key is a folder, 0 otherwise."
					},
	{"keyIsLink",py_keyIsLink,1,	"keyIsLink(key) -> int\nCheck if a key is a link key.\n"
					"Returns: 1 if key is a link, 0 otherwise."
					},
	{"keyIsBin",py_keyIsBin,1,	"keyIsBin(key) -> int\nCheck if a key is binary type.\n"
					"Returns: 1 if it is binary."
					},
	{"keyIsString",py_keyIsString,1,"keyIsString(key) -> int\nCheck if a key is a string type.\n"
					"Returns: 1 if it is not binary."
					},

	//{"keyToStream",py_keyToStream,1,"keyToStream help message"}, //TODO
	//{"keyToStreamBasename",py_keyToStreamBasename,1,"keyToStreamBasename help message"}, //TODO

	{"ksInsert",py_ksInsert,1,	"ksInsert(ks,key) inserts a new Key in the beginning of the KeySet. A reference to the key will be\n" 
					"stored, and not a copy of the key. So a future ksClose() or ksDel() on ks will keyDel() the key object.\n"
					"Parameters: \n"
					"\tks\t the keyset\n"
					"\tkey\t key that will be inserted in the keyset\n"
					"Return: size of keyset after insertion."
					},
	{"ksAppend",py_ksAppend,1,	"ksAppendKey(ks,key) appends a new Key at the end of the KeySet. A reference to the key will be stored\n"
					", and not a copy of the key. So a future ksClose() or ksDel() on ks will keyDel() the key object.\n"
					"Parameters: \n"
					"\tks\t the keyset\n"
					"\tkey\t key that will be appended in the keyset\n"
					"Return: size of keyset after insertion."
					},
	{"ksPop",py_ksPop,1,	"ksPop(ks) Remove and return the first key of ks."
				"If ks' cursor was positioned in the poped key, ks will be ksRewind()ed. "
				},
	{"ksPopLast",py_ksPopLast,1,"ksPop(ks) Remove and return the last key of ks."
				"If ks' cursor was positioned in the poped key, ks will be ksRewind()ed. "
				},
	{"ksInsertKeys",py_ksInsertKeys,1,	"ksInsertKeys(ks,toInsert) transfers all keys from toInsert to the begining of ks.\n"
						"After this call, toInsert will be empty and can be deleted with ksDel()\n"
						"Returns: the size of the KeySet after transfer."
						},
	{"ksAppend",py_ksAppend,1,	"ksAppend(ks,toAppend) Transfers all toAppend contained keys to the end of the ks\n"
						"After this call, toAppend will be empty and can be deleted with ksDel()\n"
						"Returns: the size of the KeySet after transfer."
						},
	//{"ksToStream",py_ksToStream,1,"ksToStream help message"}, // TODO
	{"ksSort",py_ksSort,1,"ksSort(ks) sorts a KeySet aphabetically by Key name, using qsort()."
				},
	{"ksLookupByString",py_ksLookupByString,1,	"ksLookupByString(ks,value,options) lookups for a Key contained in ks KeySet that\n" 
							"matches value, starting from ks' ksNext() position.\n"
							"Parameters: \n"
							"\tks\t where to look for\n"
							"\tvalue\t the value which owner key you want to find\n"
							"\toptions\t option bits\n"
							"Returns: the key found, 0 otherwise."
							},
	{"ksLookupByBinary",py_ksLookupByBinary,1,	"ksLookupByBinary(ks,value,options) lookups for a Key contained in ks KeySet that\n"
							"matches the binary value, starting from ks' ksNext() position.\n"
							"Parameters: \n"
							"\tks\t where to look for\n"
							"\tvalue\t the value which owner key you want to find\n"
							"\toptions\t option bits\n"
							"Returns: the key found, 0 otherwise."
							},
	//{"ksLookupRE",py_ksLookupRE,1,"ksLookupRE help message"},
	{"ksCurrent",py_ksCurrent,1,"ksCurrent(ks) returns the current Key or 0 if you reached the end"},
	{"ksHead",py_ksHead,1,"ksHead(ks) returns the first key in the KeySet, without changing the KeySet's internal cursor."},
	{"ksTail",py_ksTail,1,"ksTail(ks) returns the last key in the KeySet, without changing the KeySet's internal cursor."},
	{NULL,NULL,0,NULL}
};

void initlibpyelektra()
{
	(void) Py_InitModule("libpyelektra", elektra_methods);
}
