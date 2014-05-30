
import sys
import json
import pandas as pd

def main(fn, output):
    exposures = []
    outcomes = []
    with open(fn) as f:
        for line in f:
            obj = json.loads(line)
            userid = obj['inputs']['userid']

            if obj['event'] == 'exposure':
                p = obj['params']
                for position, (sk, source) in enumerate(zip(p['story_keys'], p['sources'])):
                    exposures.append({
                        'userid': userid,
                        'position': position,
                        'story_key': sk,
                        'source': source,
                        })
            elif obj['event'] == 'summary':
                for sk, summary in obj['extra_data'].iteritems():
                    outcomes.append({
                        'userid': userid,
                        'story_key': int(sk),
                        'summary': summary[0],
                    })

    e = pd.DataFrame(exposures).set_index(['userid', 'story_key'])
    o = pd.DataFrame(outcomes).set_index(['userid', 'story_key'])

    e.join(o).to_csv(output)


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
