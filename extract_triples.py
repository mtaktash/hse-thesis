import os
import argparse
import string
import codecs
import json

from nltk import DependencyGraph


LETTERS = str.maketrans('', '', string.ascii_letters)


def extract(triples0):
    sov = {}
    triples = list(triples0)
    for triple in triples:
        if triple:
            if triple[0][1] == 'VERB':
                sov[triple[0][0]] = {'verb': set(), 'obj': set(), 'subj': set()}
            if (triple[1] == 'xcomp') and triple[0][0] in sov:
                sov[triple[0][0]]['verb'].add(triple[2][0])

    for triple in triples:
        if triple:
            if (triple[1] == 'xcomp') and triple[0][0] in sov:
                sov[triple[0][0]]['verb'].add(triple[2][0])
            if triple[1] == 'neg' and triple[0][0] in sov:
                sov[triple[0][0]]['verb'].add('не')
            if triple[1] == 'nsubj' and triple[0][0] in sov:
                sov[triple[0][0]]['subj'].add(triple[2][0])
            if triple[1] == 'dobj' and triple[0][0] in sov:
                sov[triple[0][0]]['obj'].add(triple[2][0])

    for verb in sov:
        subj = sov[verb]['subj']
        subj_group = set()
        added = 1
        while added:
            added = 0
            for triple in triples:
                if (triple[1] == 'dobj'
                    or triple[1] == 'nmod') \
                        and triple[0][0] in subj \
                        and triple[2][0] not in subj_group:
                    subj_group.add(triple[2][0])
                    triples.remove(triple)
                    added = 1
                if (triple[1] == 'appos' or triple[1] == 'name') and triple[0][0] in subj:
                    subj_group.add(triple[2][0])
                    added = 1
                    triples.remove(triple)
        sov[verb]['subj'].update(subj_group)

    for verb in sov:
        obj_group = set()
        added = 1
        while added:
            added = 0
            for triple in triples:
                if (triple[1] == 'dobj' or triple[1] == 'nmod') and (triple[0][0] in subj or triple[0][0] == verb) and \
                        triple[2][0] not in obj_group:
                    triples.remove(triple)
                    obj_group.add(triple[2][0])
                    added = 1
        sov[verb]['obj'].update(obj_group)

    sov1 = {}
    for v in sov:
        if 'del' not in sov[v]['subj']:
            if 'не' not in sov[v]['verb']:
                sov1[v] = sov[v]
            else:
                sov[v]['verb'].remove('не')
                sov1['не ' + v] = sov[v]

    return (sov1)


def triples_extraction(path):
    processed_sentences = []
    sentence = []

    for line in codecs.open(path, 'r', 'utf-8'):
        if len(line) == 1:
            processed_sentences.append(sentence)
            sentence = []
        else:
            word = line.split("\t")
            sentence.append(word)

    deps = []
    for sentence in processed_sentences:
        s = u""
        for line in sentence:
            s += u"\t".join(line) + u'\n'
        deps.append(s)

    triples = []
    for sent_dep in deps:
        try:
            graph = DependencyGraph(tree_str=sent_dep)
        except:
            pass
        try:
            res = extract(graph.triples())
            triples.append(res)
        except:
            pass

    return (triples)


def get_triples(texts_path, company_name):
    out_dir = os.path.join(texts_path, 'triples', company_name, 'processed')
    sov = {}
    for filename in os.listdir(out_dir):
        if filename.startswith(company_name):
            date = filename.translate(LETTERS)
            date = date[1:-8]

            path = os.path.join(out_dir, filename)
            result = triples_extraction(path)

            for tr in result:
                for v1 in tr:
                    for cat in tr[v1]:
                        tr[v1][cat] = list(tr[v1][cat])

            if not sov.get(date):
                sov[date] = []
            sov[date].append(result)
    return sov


def main(args):
    print('Extracting triples...')
    triples = get_triples(args.texts_path, args.company_name)

    print('Writing to file...')
    with open(os.path.join(args.texts_path, f'{args.company_name}_triples.json'), 'w',  encoding='utf8') as out:
        json.dump(triples, out, indent=4, ensure_ascii=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('company_name')
    parser.add_argument('--texts-path', default='../utils/db')
    args = parser.parse_args()
    main(args)
