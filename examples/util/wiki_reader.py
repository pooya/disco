from disco.core import Job, result_iterator
from disco.worker.classic.func import chain_reader
from disco.worker.pipeline.worker import Stage
from disco.worker.task_io import chain_reader, task_input_stream
from disco.compat import str_to_bytes
import hashlib

SAVE=True

def getHash(line):
    return int(hashlib.md5(str_to_bytes(line)).hexdigest(), 16) % 128

def map(interface, state, label, inp):
    import __builtin__
    import string
    unwanted = ",!.()[]{}<>-+=|/'*:?;!@#$%^&*~`?\""
    table = string.maketrans(unwanted, ' ' * len(unwanted))
    for line in inp:
        if isinstance(line, unicode):
            effectiveLine = line.encode('utf-8')
        else:
            effectiveLine = str(line)
        words = effectiveLine.translate(table).lower()
        for word in words.split():
            interface.output(getHash(word)).add(bytes(word), str(1))

def reduce(interface, state, label, inp):
    out = interface.output(0)
    currentWord = None
    for word, one in inp:
        if currentWord == None:
            currentWord = word
            count = 1
            continue
        if word == currentWord:
            count += 1
        else:
            out.add(currentWord, count)
            currentWord = word
            count = 1

    if currentWord != None:
        out.add(currentWord, count)

class CountWords(Job):
    save = True
    def __init__(self):
        from disco.worker.pipeline.worker import Worker
        super(CountWords, self).__init__(worker = Worker())
    pipeline = [("split", Stage("map", process=map,
                input_chain = [task_input_stream, chain_reader])),
                ("group_label", Stage("reduce", process=reduce, combine=True,
                    sort=True, save_results=SAVE))]

def getHdfsMaster(discoMaster):
    from disco.util import schemesplit
    _, rest = schemesplit(discoMaster)
    return rest.split(':')[0] + ':50070'

if __name__ == '__main__':
    import sys
    if len(sys.argv) != 2:
        sys.exit(1)
    from wiki_reader import CountWords
    job = CountWords()
    job.save = True
    job.save_results = True
    job.save_info = "hdfs," + getHdfsMaster(job.disco.master) + ",shayan,/user/shayan/"
    job.run(input=["tag://" + sys.argv[1]])

    if SAVE:
        job.wait(show=True)
    else:
        for line, count in result_iterator(job.wait(show=False)):
            print(line, count)
