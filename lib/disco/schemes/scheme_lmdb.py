from disco.worker.task_io import task_output_stream
from disco.util import schemesplit
import mdb
import os

class LmdbOutputStream(file):
    def __init__(self, path, db_name):
        self.path = path
        self.db_name = db_name
        from collections import defaultdict
        self.Dict = defaultdict(list)

    def add(self, k, v):
        self.Dict[k].append(v)

    def close(self):
        env, txn, db = mdb.mdb_write_handle(self.path, 1*mdb.MB, db_name=self.db_name,
                                open_flags=mdb.MDB_NOSUBDIR|mdb.MDB_NOLOCK)

        for k, vs in self.Dict.items():
            for v in vs:
                db.put(txn, k, v)
        txn.commit()
        db.close()
        env.close()

def lmdb_output(stream, partition, url, params, db_name):
    return LmdbOutputStream(url, db_name=db_name), 'lmdb:{0}'.format(url.split(':', 1)[1])

def lmdb_inter_stream_out(stream, partition, url, params, db_name):
    path = os.path.realpath(url)
    print "path is: ", path, " url is: ", url
    return LmdbOutputStream(path, db_name=db_name)

class LmdbStream(file):
    def __init__(self, path, db_name):
        self.env, self.txn, self.db = mdb.mdb_read_handle(path, db_name=db_name,
                open_flags=mdb.MDB_NOSUBDIR|mdb.MDB_NOLOCK)
        self.generator = self.db.items(self.txn)
        self.path = path

    def __iter__(self):
        for item in self.generator:
            yield item

    def __len__(self):
        print "len called"
        return 10 #TODO

    def read(self, num_bytes=None):
        print "num_bytes is: ", num_bytes
        return [self.__iter__()]

def lmdb_inter_stream_in(stream, size, url, params, db_name):
    from disco import comm, util
    disco_data = "/Users/shayan/workspace/disco/data"
    _scheme, _netloc, path = util.urlsplit(url)
    prefix, fname = path.split('/', 1)
    Path = os.path.join(disco_data, fname)
    print "path is: ", Path
    file = LmdbStream(Path, db_name=db_name)
    return file, len(file), file.path

if __name__ == '__main__':
    import sys

    File = sys.argv[1]
    r = LmdbStream(File, "test")
    for i in r:
        print i
#    env, txn, db = mdb.mdb_read_handle(File, db_name="test",
#                            open_flags=mdb.MDB_NOSUBDIR|mdb.MDB_NOLOCK)
#    for i in db.items(txn):
#        print i
