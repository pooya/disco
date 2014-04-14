from disco.worker.task_io import task_output_stream
from disco.util import schemesplit
import mdb

class LmdbOutputStream(object):
    def __init__(self, stream, url):
        import os
        dir = os.path.dirname(os.path.realpath(url))
        self.env = mdb.Env(dir, max_dbs=8)
        from collections import defaultdict
        self.Dict = defaultdict(list)
        # this is for input
        #self.generator = self.db.items(txn)

    def add(self, k, v):
        self.Dict[k].append(v)

    def close(self):
        self.txn = self.env.begin_txn()
        self.db = self.env.open_db(self.txn, "test_write")
        for k, vs in self.Dict.items():
            for v in vs:
                self.db.put(self.txn, k, v)
        self.txn.commit()
        self.db.close()
        self.env.close()

def lmdb_output(stream, partition, url, params):
    return LmdbOutputStream(stream, url), 'lmdb:{0}'.format(url.split(':', 1)[1])

lmdb_stream = (task_output_stream, lmdb_output)

if __name__ == '__main__':
    import sys

    File = sys.argv[1]
    env = mdb.Env(File, max_dbs=8)
    txn = env.begin_txn()
    db = env.open_db(txn, "test_write")
    for i in db.items(txn):
        print i
