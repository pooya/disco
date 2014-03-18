from disco.test import TestCase, TestJob
from disco.worker.task_io import task_output_stream

class DiscoPlainOut(object):
    def __init__(self, stream):
        self.stream = stream

    def add(self, k, v):
        k, v = str(k), str(v)
        self.stream.write("%d %s %d %s\n" % (len(k), k, len(v), v))

    def close(self):
        pass

def plain_output_stream(stream, partition, url, params):
    return DiscoPlainOut(stream)

class SaveMapJob(TestJob):
    partitions = None
    save = True
    save_info = "hdfs,devdisco03:50070,shayan,/user/shayan/"
    map_output_stream= (task_output_stream, plain_output_stream)

    @staticmethod
    def map(e, params):
        yield e.strip() + b'!', ''

class SaveTestCase(TestCase):
    def serve(self, path):
        return '{0}\n'.format(path)

    def test_save_map(self):
        input = range(10)
        self.job = SaveMapJob().run(input=self.test_server.urls(input))
        results = sorted(self.results(self.job))
        self.tag = self.disco.results(self.job.name)[1][0]
