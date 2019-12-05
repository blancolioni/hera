import { ClientState } from '../clients/model';

export interface SurfacePoint {
  x : number,
  y : number,
  z : number,
}

export interface Sector {
  color: string,
  normal: SurfacePoint,
  boundary: SurfacePoint[],
}

export interface State extends ClientState {
  id : string,
  name : string,
  sectors : Sector[];
}

export const planetInitialState = (baseState : ClientState) : State => ({
  ...baseState,
  id: '',
  name: '',
  sectors: [],
});
