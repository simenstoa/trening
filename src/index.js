import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Main.embed(document.getElementById('root'), {
  clientSecret: process.env.ELM_APP_STRAVA_CLIENT_SECRET,
  clientId: process.env.ELM_APP_STRAVA_CLIENT_ID,
});

registerServiceWorker();
