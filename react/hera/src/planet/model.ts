import { ClientState } from '../clients/model';

export interface Sector {
  id: string,
  x: number,
  y: number,
  terrain : string,
}

export interface State extends ClientState {
  sectors : Sector[];
  width : number,
  height : number,
}

export const planetInitialState = (baseState : ClientState) : State => ({
  ...baseState,
  width: 0,
  height: 0,
  sectors: [],
});
