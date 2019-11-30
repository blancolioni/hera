import { ClientState } from './clients/model'
import { shellInitialState } from './shell/model'
import { tableInitialState } from './table/model';
import { worldInitialState } from './world/model';
import { planetInitialState } from './planet/model';
import { systemInitialState } from './system/model';
import { galaxyInitialState } from './galaxy/model';

interface Model {
    create   : (baseState : ClientState) => ClientState
}

interface ModelTable {
    [key : string] : Model
}

const modelTable : ModelTable = {
    Shell: {
        create: shellInitialState,
    },
    Table: {
        create: tableInitialState,
    },
    World: {
        create: worldInitialState,
    },
    Planet: {
        create: planetInitialState,
    },
    System: {
        create: systemInitialState,
    },
    Galaxy: {
        create: galaxyInitialState,
    },
}

export function initialState (modelName : string, baseState : ClientState) : ClientState {
    return modelTable[modelName].create(baseState);
}