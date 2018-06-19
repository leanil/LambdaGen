import { SyntaxTreeNodeModel } from "./SyntaxTreeNodeModel";
import { ISyntaxTreeNodeProps, SyntaxTreeNodeWidget } from "./SyntaxTreeNodeWidget";

import * as React from "react";
import * as SRD from "storm-react-diagrams";

export class SyntaxTreeNodeFactory<ModelT extends SyntaxTreeNodeModel, WidgetT extends SyntaxTreeNodeWidget<ModelT>>
       extends SRD.AbstractNodeFactory<ModelT>
{
	constructor(private Model: new () => ModelT, private Widget: new (props: ISyntaxTreeNodeProps<ModelT>) => WidgetT, nodeType: string) {
		super(nodeType);
	}

	public generateReactWidget(diagramEngine: SRD.DiagramEngine, node: ModelT): JSX.Element {
        return <this.Widget node={node} diagramEngine={diagramEngine} />;
	}

	public getNewInstance() {
		return new this.Model();
    }
}