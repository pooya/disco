from disco.worker.task_io import task_output_stream
from disco.util import schemesplit
import mdb
import os

class LmdbOutputStream(file):
    def __init__(self, path, dbname):
        import os
        self.path = path
        self.dbname = dbname
        from collections import defaultdict
        self.Dict = defaultdict(list)

    def add(self, k, v):
        self.Dict[k].append(v)

    def close(self):
        env, txn, db = mdb.mdb_write_handle(self.path, 1*mdb.MB, db_name=self.dbname,
                                open_flags=mdb.MDB_NOSUBDIR|mdb.MDB_NOLOCK)

        for k, vs in self.Dict.items():
            for v in vs:
                db.put(txn, k, v)
        txn.commit()
        db.close()
        env.close()

def lmdb_output(stream, partition, url, params, name):
    return LmdbOutputStream(url, name), 'lmdb:{0}'.format(url.split(':', 1)[1])

def lmdb_inter_stream_out(stream, partition, url, params, dbname):
    return LmdbOutputStream(path, dbname=dbname)

class LmdbStream(file):
    def __init__(self, url, name):
        self.path = path
        import os
        dir = os.path.dirname(os.path.realpath(path))
        self.env = mdb.Env(dir, max_dbs=8)
        self.txn = self.env.begin_txn()
        self.db = self.env.open_db(self.txn, name)
        self.generator = self.db.items(self.txn)

    def __iter__(self):
        yield self.generator.next()

    def __len__(self):
        return 0

    def read(self, num_bytes=None):
        return [self.__iter__()]

def lmdb_inter_stream_in(stream, partition, url, params, path, name):
    file = LmdbStream(path, name)
    return file, len(file), file.path

if __name__ == '__main__':
    import sys

    File = sys.argv[1]
    env, txn, db = mdb.mdb_read_handle(File, db_name="test",
                            open_flags=mdb.MDB_NOSUBDIR|mdb.MDB_NOLOCK)
    for i in db.items(txn):
        print i
