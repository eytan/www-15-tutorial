
import json
from uuid import uuid4
from itertools import islice, cycle
from argparse import ArgumentParser, FileType

from tornado.ioloop import IOLoop
from tornado.web import Application, RequestHandler, StaticFileHandler
from tornado.httpserver import HTTPServer

from planout.experiment import SimpleExperiment
from planout.ops.random import *

class CueExperiment(SimpleExperiment):
    """
    Randomly reorder the stories and choose from available cues.
    """
    def setup(self):
        self.set_log_file('webapp/logs/experiment.log')

    def assign(self, params, userid, story_keys, n):
        params.story_keys = Sample(choices=story_keys, unit=userid, draws=n)
        balanced_sources = list(islice(cycle([
            'msnbc',
            'cnn',
            'foxnews',
        ]), n))
        params.sources = Sample(choices=balanced_sources, unit=userid, draws=n)

class Likert(SimpleExperiment):
    """
    A randomization of the survey scale for robustness of the politics measure.
    This illustrates how PlanOut can be used to randomize scales in surveys.
    """

    def setup(self):
        self.set_log_file('webapp/logs/survey.log')

    def assign(self, params, userid):
        params.reversed_scale = UniformChoice(choices=[0,1], unit=userid)


class BaseRequestHandler(RequestHandler):
    def get_current_user(self):
        userid = self.get_secure_cookie('userid')
        if not userid:
            userid = str(uuid4())
            self.set_secure_cookie('userid', userid)
        return userid

class PoliticalSurvey(BaseRequestHandler):
    
    scale = list(enumerate([
            'very conservative',
            'conservative',
            'moderate',
            'liberal',
            'very liberal',
        ]))

    def get(self):
        if self.get_secure_cookie('survey_taken'):
            pass #self.redirect('/thanks')

        exp = Likert(userid=self.current_user)

        if exp.get('reversed_scale'):
            scale = list(reversed(self.scale))
        else:
            scale = self.scale

        self.render(
            'templates/survey.html',
            scale=scale
        )

    def post(self):

        resp = self.get_argument('politics', None)
        if resp is None:
            self.redirect('/survey')
        else:
            resp = int(resp)

        exp = Likert(userid=self.current_user)

        exp.log_event('response', {'resp': resp})

        self.set_secure_cookie('survey_taken', '1')
        self.redirect('/thanks')

class Thanks(BaseRequestHandler):
    def get(self):
        if self.get_secure_cookie('survey_taken') == '1':
            userid = self.get_secure_cookie('userid')
            code = hash(userid) % 1000000
        else:
            code = None

        self.render('templates/thanks.html',
                    code=code)

class SummariseContent(BaseRequestHandler):
    

    def initialize(self, stories, k, n):
        self.stories = stories
        self.story_keys = sorted(stories.keys())
        self.k = k
        self.n = n
 
    def get(self):
        exp = CueExperiment(
            userid=self.current_user,
            story_keys=self.story_keys,
            n=self.n,
        )

        stories = [(k, self.stories[k]) for k in exp.get('story_keys')]
        self.render(
            'templates/story.html', 
            k=self.k,
            n=self.n,
            sources=exp.get('sources'),
            stories=stories,
        )

    def post(self):
        exp = CueExperiment(
            userid=self.current_user,
            story_keys=self.story_keys,
            n=self.n,
        )
        exp.log_event('summary', self.request.arguments)
        self.redirect('/survey')


if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument('--n', default=10, type=int, 
                        help='How many stories to show.')
    parser.add_argument('--k', default=3, type=int,
                        help='How many the subject should select.')
    parser.add_argument('--stories', default='webapp/stories.json', 
                        type=FileType('r'),
                        help='Source of stories.')
    parser.add_argument('--port', default=9000, type=int)
    parser.add_argument('--host', default='127.0.0.1')
    parser.add_argument('--debug', default=False, action='store_true')
    args = parser.parse_args()

    stories = {int(k): v for k, v in json.load(args.stories).iteritems()}

    app = Application([
        (r'/', SummariseContent, {'stories': stories, 
                                  'n': args.n, 
                                  'k': args.k}),
        (r'/survey', PoliticalSurvey),
        (r'/thanks', Thanks),
        (r'/static/(.*)', StaticFileHandler, {'path': 'static'})
    ],
        debug=args.debug,
        static_path='webapp/static',
        cookie_secret='0.27522903039811131',
    )
    http_server = HTTPServer(app)
    http_server.listen(args.port, address=args.host)
    IOLoop.instance().start()
