import { combineReducers } from 'redux';
import login from './login';
import toolbar from './toolbar';
import dashboard from './dashboard';
import clients from './clients';
import planet from './planet';
import galaxy from './galaxy';
import shell from './shell';
import system from './system';
import table from './table';
import world from './world';
import outline from './outline';

const rootReducer = combineReducers({
  [login.constants.NAME]: login.reducer,
  [toolbar.constants.NAME]: toolbar.reducer,
  [outline.constants.NAME]: outline.reducer,
  [dashboard.constants.NAME]: dashboard.reducer,
  [clients.constants.NAME]: clients.reducer([shell.reducer, table.reducer, world.reducer, system.reducer, galaxy.reducer, planet.reducer]),
});

export default rootReducer
export type AppState = ReturnType<typeof rootReducer>