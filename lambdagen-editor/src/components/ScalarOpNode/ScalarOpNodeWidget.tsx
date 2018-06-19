import { ISyntaxTreeNodeProps, SyntaxTreeNodeWidget } from "../SyntaxTreeNode/SyntaxTreeNodeWidget";

import * as React from "react";
import { ScalarOpNodeModel } from "./ScalarOpNode";

export class ScalarOpNodeWidget extends SyntaxTreeNodeWidget<ScalarOpNodeModel> {

    constructor(props: ISyntaxTreeNodeProps<ScalarOpNodeModel>) {
        super(props);
        this.props.node.value = '+';
        this.state = {value: '+'};
        this.handleChange = this.handleChange.bind(this);
    }

    public handleChange(event: any) {
        this.props.node.value = event.target.value;
        this.setState({value: event.target.value});
    }
    
    public properties() {
        return  <select value={this.state.value} onChange={this.handleChange}>
                    <option value="+">+</option>
                    <option value="*">*</option>
                </select>;
    }
}