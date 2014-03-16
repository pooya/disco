from disco.ddfs import DDFS
from disco.job import JobChain
from disco.test import TestCase, TestJob
from disco.worker.task_io import task_output_stream
from disco.compat import str_to_bytes
from disco.worker.task_io import ClassicFile

class SaveMapJob(TestJob):
    partitions = None
    save = False
    save_info = "hdfs,devdisco03:50070,shayan,/user/shayan/"

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
