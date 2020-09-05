/**
 * @file
 *
 * @brief connect the TreeSearch component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {connect} from "react-redux";
import {bindActionCreators} from "redux";

import {clearSearch, findKey, sendNotification} from "../actions";
import TreeSearch from "../components/TreeSearch";

const mapStateToProps = state => {
	return { search: state.kdbFind };
};

const mapDispatchToProps = dispatch => bindActionCreators ({ findKey, clearSearch, sendNotification }, dispatch);

export default connect (mapStateToProps, mapDispatchToProps) (TreeSearch);
