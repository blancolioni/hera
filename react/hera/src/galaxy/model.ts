import { ClientState } from '../clients/model';

export interface GalaxyObject {
  id: string,
  name: string,
  mass: number,
  temperature : number,
  x : number,
  y : number,
  z : number,
  color : string,
  colonized: boolean,
}

export interface State extends ClientState {
  zoom : string,
  systems : GalaxyObject[],
}

export const galaxyInitialState = (baseState : ClientState) : State => ({
  ...baseState,
  zoom: '',
  systems: [],
});
