import os
import argparse
import pandas as pd
import datetime as dt
import random
import string

from multiprocessing import Pool
from nltk.tokenize import sent_tokenize
from tqdm import tqdm

from syntaxnet_wrapper import ProcessorSyntaxNet

PUNCTUATION = (string.punctuation + '–—«»‘’“”').replace('-', '').replace(',', '').replace('.', '')
PUNCTUATION_TRANS_TABLE = str.maketrans('', '', PUNCTUATION)

SYNTAXNET_HOST = '104.236.250.132'
SYNTAXNET_HOSTS = ['104.236.250.132', '206.189.62.7', '45.55.130.238', '45.55.131.21']
SYNTAXNET_PORT = 8111

syntaxnet_proc = None


def load_raw_texts(path):
    texts = []
    for filename in ['rbc.csv', 'vedomosti.csv']:
        news = pd.read_csv(os.path.join(path, filename), parse_dates=['date'])
        news['source'] = filename[:-4]
        texts.append(news)

    texts = pd.concat(texts)
    texts = texts[texts['date'] >= dt.datetime(2011, 1, 1)]
    texts.dropna(subset=['title'], inplace=True)
    texts['text'] = texts['text'].apply(lambda x: '' if pd.isnull(x) else x)
    return texts


def preprocess_texts(company_name, company_keywords, company_stopwords, texts_path):
    """Loads texts from vedomosti and rbc, filters news for a company and saves them for syntaxnet"""
    texts = load_raw_texts(texts_path)

    out_dir = os.path.join(texts_path, 'triples', company_name)
    os.makedirs(out_dir, exist_ok=True)

    for i, row in tqdm(texts.iterrows(), total=len(texts)):
        text = row.title + ' ' + row.text
        text = text.translate(PUNCTUATION_TRANS_TABLE)
        text = text.replace(',', ' ,')

        date = row.date.strftime('%Y-%m-%d_%H-%M')
        source = row.source
        filename = f'{company_name}_{date}_{source}.csv'
        out_path = os.path.join(out_dir, filename)

        if not os.path.isfile(out_path):
            if any(word in text for word in company_keywords):
                if any(stopword in text for stopword in company_stopwords):
                    continue

                sentences = sent_tokenize(text)
                with open(out_path, 'w') as out:
                    for sent in sentences:
                        sent = sent[:-1] + ' .\n'
                        out.write(sent)


def get_raw_sov_for_filename(file_path: str):
    in_path = file_path
    out_path = os.path.join(
        file_path[:file_path.rfind('/')],
        'processed', file_path[file_path.rfind('/') + 1:-4] + '.conll'
    )
    if not file_path.endswith('.csv'):
        return
    if not os.path.isfile(out_path):
        with open(in_path, 'r') as in_file:
            text = in_file.read()
        try:
            if syntaxnet_proc is not None:
                result = syntaxnet_proc.parse(text, raw_output=True)
            else:
                proc = ProcessorSyntaxNet(random.choice(SYNTAXNET_HOSTS), SYNTAXNET_PORT)
                result = proc.parse(text, raw_output=True)
        except (UnicodeDecodeError, TimeoutError, ConnectionRefusedError):
            print(file_path)
            return None
        with open(out_path, 'w') as out_file:
            if result is not None:
                out_file.write(result)
            else:
                out_file.write('')
        # os.system(f'cat {in_path} | docker run -a stdout > log.txt --rm -i inemo/syntaxnet_rus > {out_path}')


def get_raw_sov(company_name, texts_path):
    in_dir = os.path.join(texts_path, 'triples', company_name)
    out_dir = os.path.join(texts_path, 'triples', company_name, 'processed')
    os.makedirs(out_dir, exist_ok=True)

    file_paths = [os.path.join(in_dir, x) for x in os.listdir(in_dir)]
    with Pool(4) as pool:
        for _ in tqdm(pool.imap(get_raw_sov_for_filename, file_paths), total=len(file_paths)):
            pass


def main(args):
    company_keywords = [x for x in args.company_keywords.split()]
    company_stopwords = [x for x in args.company_stopwords.split()]

    if args.host != SYNTAXNET_HOST:
        global syntaxnet_proc
        syntaxnet_proc = ProcessorSyntaxNet(args.host, int(args.port))

    if args.preprocess:
        print('Preprocessing texts...')
        preprocess_texts(
            args.company_name,
            company_keywords,
            company_stopwords,
            args.texts_path
        )

    print('Parsing with syntaxnet...')
    get_raw_sov(args.company_name, args.texts_path)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('preprocess', default=0, type=int)
    parser.add_argument('--company-name', default='magnit')
    parser.add_argument('--texts-path', default='../utils/db')
    parser.add_argument('--company-keywords')
    parser.add_argument('--company-stopwords')
    parser.add_argument('--host', default=SYNTAXNET_HOST)
    parser.add_argument('--port', default=SYNTAXNET_PORT)
    args = parser.parse_args()
    main(args)
