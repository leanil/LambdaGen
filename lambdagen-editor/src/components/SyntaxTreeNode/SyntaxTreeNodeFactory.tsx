import { SyntaxTreeNodeModel } from "./SyntaxTreeNodeModel"
import { SyntaxTreeNodeWidget } from "./SyntaxTreeNodeWidget"

import * as React from "react";
import * as SRD from "storm-react-diagrams";

export class SyntaxTreeNodeFactory extends SRD.AbstractNodeFactory<SyntaxTreeNodeModel> {
	constructor() {
		super("syntaxTree");
	}

	public generateReactWidget(diagramEngine: SRD.DiagramEngine, node: SyntaxTreeNodeModel): JSX.Element {
		return <SyntaxTreeNodeWidget node={node} diagramEngine={diagramEngine} />;
	}

	public getNewInstance() {
		return new SyntaxTreeNodeModel();
    }
}