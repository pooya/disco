from disco.core import Job, result_iterator

def map(line, params):
    for word in line.split():
        yield word, 1

def reduce(iter, params):
    from disco.util import kvgroup
    for word, counts in kvgroup(sorted(iter)):
        yield word, sum(counts)

def getHdfsMaster(discoMaster):
    from disco.util import schemesplit
    _, rest = schemesplit(discoMaster)
    return rest.split(':')[0] + ':50070'

if __name__ == '__main__':
    job = Job()
    hdfsMaster = getHdfsMaster(job.disco.master)
    job = job.run(input=['hdfs://' + hdfsMaster + '://user/shayan/chekhov'],
                    map=map,
                    reduce=reduce)
    for word, count in result_iterator(job.wait(show=True)):
        print(word, count)
