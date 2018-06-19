import { ISyntaxTreeNodeProps, SyntaxTreeNodeWidget } from "../SyntaxTreeNode/SyntaxTreeNodeWidget";
import { ScalarNodeModel } from "./ScalarNode";

import * as React from "react";

export class ScalarNodeWidget extends SyntaxTreeNodeWidget<ScalarNodeModel> {

    constructor(props: ISyntaxTreeNodeProps<ScalarNodeModel>) {
        super(props);
        this.state = {value: ''};
        this.handleChange = this.handleChange.bind(this);
    }

    public handleChange(event: any) {
        this.props.node.value = parseFloat(event.target.value);
        this.setState({value: event.target.value});
    }
    
    public properties() {
        return <input type="number" value={this.state.value} style={{width: this.state.value.length+2+"ch"}}
                onChange={this.handleChange} onKeyUp={event => {event.stopPropagation();}} />;
    }
}