from disco.worker.task_io import task_output_stream
from disco.util import schemesplit
import mdb

class LmdbOutputStream(object):
    def __init__(self, stream, url):
        #print "url is: ", url, "stream is: ", stream, "stream.path is: ", stream.path
        import os
        dir = os.path.dirname(os.path.realpath(url))
        self.env = mdb.Env(dir, max_dbs=8)
        self.txn = self.env.begin_txn()
        self.db = self.env.open_db(self.txn, "test_write")
        # this is for input
        #self.generator = self.db.items(txn)

    def add(self, key, value):
        self.db.put(self.txn, key, value)

    def close(self):
        self.txn.commit()
        self.db.close()
        self.env.close()

def lmdb_output(stream, partition, url, params):
    return LmdbOutputStream(stream, url), 'lmdb:{0}'.format(url.split(':', 1)[1])

lmdb_stream = (task_output_stream, lmdb_output)

if __name__ == '__main__':
    File = "/Users/shayan/workspace/disco/data/localhost/eb/WordCount@575:790f4:35ba9/0d/reduce:0--2-5a-66272"
    env = mdb.Env(File, max_dbs=8)
    txn = env.begin_txn()
    db = env.open_db(txn, "test_write")
    for i in db.items(txn):
        print i
