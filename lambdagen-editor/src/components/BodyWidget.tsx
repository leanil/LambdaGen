import * as React from "react";
import * as SRD from "storm-react-diagrams";
import { DiagramWidget } from "storm-react-diagrams";
import { ScalarNodeModel } from "./ScalarNode/ScalarNode";
import { ScalarOpNodeModel } from "./ScalarOpNode/ScalarOpNode";

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
					if (data.type === "Scalar") {
						node = new ScalarNodeModel();
					} else {
						node = new ScalarOpNodeModel();
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