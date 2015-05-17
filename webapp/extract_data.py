
import sys
import json
import pandas as pd
from argparse import ArgumentParser, FileType

def main(args):
    results = []
    for line in args.infile:
        obs = json.loads(line)

        # Make up an integer useid for convenience.
        userid = hash(obs['inputs']['userid']) % 100000
        time = obs['time']

        if obs['event'] == 'exposure' == args.event:
            for exposure in extract_exposures(obs, userid, time):
                results.append(exposure)

        elif obs['event'] == 'summary' == args.event:
            for outcome in extract_outcomes(obs, userid, time):
                results.append(outcome)

        elif obs['event'] == 'response' == args.event:
            for response in extract_survey_response(obs, userid, time):
                results.append(response)

    pd.DataFrame(results).to_csv(args.outfile, index=False)

def extract_exposures(obs, userid, time):
    p = obs['params']
    for position, (sk, source) in enumerate(zip(p['story_keys'], p['sources'])):
        yield {
            'userid': userid,
            'time': time,
            'position': position,
            'story_key': sk,
            'source': source,
            }
    
def extract_outcomes(obs, userid, time):
    for sk, summary in obs['extra_data'].iteritems():
        yield {
            'userid': userid,
            'time': time,
            'story_key': int(sk),
            'summary': int(len(summary[0]) > 0),
        }

def extract_survey_response(obs, userid, time):
    yield {
        'userid': userid,
        'time': time,
        'response': obs['extra_data']['resp'],
        'reversed_scale': obs['params']['reversed_scale'],
    }

if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument('--event')
    parser.add_argument('--infile', type=FileType('r'))
    parser.add_argument('--outfile', type=FileType('w'))

    args = parser.parse_args()
    main(args)
