from disco.core import Job, result_iterator
from disco.worker.task_io import task_output_stream, plain_output_stream

def getHdfsMaster(discoMaster):
    from disco.util import schemesplit
    _, rest = schemesplit(discoMaster)
    return rest.split(':')[0] + ':50070'

class WordCount(Job):
    save = False
    reduce_output_stream = (task_output_stream, plain_output_stream)

    @staticmethod
    def map(line, params):
        for word in line.split():
            yield word, 1

    @staticmethod
    def reduce(iter, params):
        from disco.util import kvgroup
        for word, counts in kvgroup(sorted(iter)):
            yield word, sum(counts)

if __name__ == '__main__':
    from wordcount_hdfs import WordCount
    job = WordCount()
    job.save_info = "hdfs," + getHdfsMaster(job.disco.master) + ",shayan,/user/shayan/"
    hdfsMaster = getHdfsMaster(job.disco.master)
    job = job.run(input=['hdfs://' + hdfsMaster + '://user/shayan/chekhov'])
    for word, count in result_iterator(job.wait(show=True)):
        print(word, count)
