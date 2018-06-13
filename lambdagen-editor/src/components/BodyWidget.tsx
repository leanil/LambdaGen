import * as React from "react";
import * as SRD from "storm-react-diagrams";
import { DefaultNodeModel, DiagramWidget } from "storm-react-diagrams";
import { SyntaxTreeNodeModel } from "./SyntaxTreeNode/SyntaxTreeNodeModel"

export interface IBodyWidgetProps {
	engine: SRD.DiagramEngine;
	model: SRD.DiagramModel;
}

class BodyWidget extends React.Component<IBodyWidgetProps, any> {

	constructor(props: IBodyWidgetProps) {
		super(props)
	}

	public render() {
		return (
			<div
				className="diagram-layer"
				onDrop={event => {
					const data = JSON.parse(event.dataTransfer.getData("storm-diagram-node"));
					let node = null;
					if (data.type === "Const") {
						node = new DefaultNodeModel("Const", "rgb(192,255,0)");
						node.addOutPort("result")
					} else {
						// node = new DefaultNodeModel("ScalarOp", "rgb(0,192,255)");
						node = new SyntaxTreeNodeModel();
						// node.addInPort("a");
						// node.addInPort("b");
					}
					const points = this.props.engine.getRelativeMousePoint(event);
					node.x = points.x;
					node.y = points.y;
					this.props.model.addNode(node);
					this.forceUpdate();
				}}
				onDragOver={event => {
					event.preventDefault();
				}}
			>
				<DiagramWidget className="srd-demo-canvas" diagramEngine={this.props.engine} />
			</div>
		);
	}
}

export default BodyWidget;