import React from "react";
import * as THREE from "three";
import { noise4d } from '../../_3d/Noise';
import { State, GalaxyObject } from '../model';
import { ClientDispatch } from '../../clients/model';
import Model3D from '../../_3d/Model3D';
import { worldMesh } from "../../world/components/World";
import { CSS2DObject } from "three/examples/jsm/renderers/CSS2DRenderer";

interface Dispatch extends ClientDispatch {
}

interface Props {
    clientState : State,
    clientDispatch : Dispatch
}

class Component extends React.Component<Props,State> {

  mount: any;
  model  : Model3D | null;
  renderCount : number
  currentZoom : THREE.Mesh | null;
  starTexture : THREE.Texture | null = null;

  constructor(props : Props) {
    super (props);

    this.renderCount = 0;
    this.model = null;
    this.currentZoom = null;
    this.beforeRender = this.beforeRender.bind(this);
  }

  componentDidMount() {
    this.model = new Model3D(this.mount, 1, 200, 1, 100);
    this.starTexture = this.model.textureLoader.load("textures/galaxy/star.png");
    this.addCustomSceneObjects();
    this.model.startAnimationLoop(this.beforeRender);
  }

  componentWillUnmount() {
    this.model!.stopAnimationLoop();
  }

  addCustomSceneObjects = () => {
    this.model!.camera.position.z = 20;
    this.addSkybox();
  }

  addSkybox = () => {
    let materialArray = [];
    for (let i=1; i <= 6; ++i) {
      let texture = this.model!.textureLoader.load('textures/galaxy/skybox/' + i + '.png');
      materialArray.push(new THREE.MeshBasicMaterial( { map: texture }));
    }
       
    for (let i = 0; i < 6; i++)
      materialArray[i].side = THREE.BackSide;
       
    let skyboxGeo = new THREE.BoxGeometry( 1000, 1000, 1000);
    let skybox = new THREE.Mesh( skyboxGeo, materialArray );
    this.model!.scene.add( skybox );
  }

  starMesh = (obj : GalaxyObject) : THREE.Sprite => {
    const material = new THREE.SpriteMaterial( { map: this.starTexture, color: obj.color } );
    const star = new THREE.Sprite(material);
    star.position.set(obj.x, obj.y, obj.z);
    star.name = obj.name;
    var starDiv = document.createElement( 'div' );
    starDiv.className = obj.colonized ? 'colonized-star-label' : 'standard-star-label';
    starDiv.textContent = star.name;
    starDiv.style.marginTop = '-1em';
    var starLabel = new CSS2DObject( starDiv );
    starLabel.position.set( 0.5, -0.1, 0 );
    star.add( starLabel );
    return star;
  }

  beforeRender() {
    this.renderCount += 0.0002;
  }

  addObject = (obj : GalaxyObject) => {

    this.model!.scene.add( this.starMesh (obj) );

  }

  updateScene = (obj : GalaxyObject) => {
    if (!this.model!.scene.getObjectByName(obj.name)) {
      this.addObject(obj);
    }
  }

  render() {

    if (this.props.clientState.systems) {
      for (const obj of this.props.clientState.systems) {
        this.updateScene(obj);
      }
    }

    if (this.props.clientState.zoom && (!this.currentZoom || this.currentZoom.name !== this.props.clientState.zoom)) {
      const newZoom = this.model!.scene.getObjectByName(this.props.clientState.zoom);
      if (newZoom) {
        this.currentZoom = newZoom as THREE.Mesh;
        const mesh = this.currentZoom;
        const { x, y, z } = mesh.position;
        const d = mesh.scale.z;
        const n = new THREE.Vector3(x, y, z).normalize().multiplyScalar(d * 3);
        const wp = new THREE.Vector3 (x, y, z).sub(n);
        
        console.log('add waypoint', x, y, z, mesh.scale.z, wp)
        this.model!.addWaypoint(wp, new THREE.Vector3 (x, y, z), 5.0);
      }
    }

    return (
      <div className="harriet-model3d" ref={ref => (this.mount = ref)} />
    )
  }
}

export default Component;
