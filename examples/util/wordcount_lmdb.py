"""
This example could be run and the results printed from the `examples/util` directory in Disco:

disco run wordcount_ddb.WordCount http://discoproject.org/media/text/chekhov.txt
"""
from disco.core import Job
from disco.util import kvgroup
from disco.schemes.scheme_lmdb import lmdb_output
from disco.worker.task_io import task_output_stream
from functools import partial

class WordCount(Job):
    reduce_output_stream = (task_output_stream, partial(lmdb_output, name="test"))

    @staticmethod
    def map(line, params):
        for word in line.split():
            yield word, 1

    @staticmethod
    def reduce(iter, params):
        for word, counts in kvgroup(sorted(iter)):
            yield word, str(sum(counts))
