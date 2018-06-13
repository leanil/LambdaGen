import BodyWidget from "./components/BodyWidget"
import TrayItemWidget from "./components/TrayItemWidget";
import TrayWidget from "./components/TrayWidget";

import { SyntaxTreeNodeFactory } from "./components/SyntaxTreeNode/SyntaxTreeNodeFactory"

import * as React from 'react';
import * as SRD from "storm-react-diagrams";

import 'storm-react-diagrams/dist/style.min.css';
import './App.css';


class App extends React.Component<any,any> {

  protected engine: SRD.DiagramEngine;
  protected model: SRD.DiagramModel;

	constructor(props: any) {
    super(props)
		this.engine = new SRD.DiagramEngine();
		this.engine.installDefaultFactories();
    this.model = new SRD.DiagramModel();
    this.engine.setDiagramModel(this.model);
    this.engine.registerNodeFactory(new SyntaxTreeNodeFactory());
    this.handleGenerate = this.handleGenerate.bind(this);
  }
  
  public handleGenerate() {
    const socket = new WebSocket('ws://localhost:2103');
    socket.addEventListener('open', event => {
      socket.send(JSON.stringify(this.model.serializeDiagram()));
      socket.close();
    });
  }

  public render() {
    return (
      <div className="body">
				<div className="header">
					<div className="title">LambdaGen editor</div>
				</div>
				<div className="content">
					<TrayWidget handleGenerate={this.handleGenerate}>
						<TrayItemWidget model={{ type: "Const" }} name="Const" color="rgb(192,255,0)" />
						<TrayItemWidget model={{ type: "ScalarOp" }} name="ScalarOp" color="rgb(0,192,255)" />
					</TrayWidget>
          <BodyWidget engine={this.engine} model={this.model} />
        </div>
      </div>
    );
  }
}

export default App;
