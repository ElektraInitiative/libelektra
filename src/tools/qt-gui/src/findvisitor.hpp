#ifndef FINDVISITOR_HPP
#define FINDVISITOR_HPP

#include "visitor.hpp"
#include "confignode.hpp"

class FindVisitor : public Visitor
{
public:
	explicit FindVisitor(TreeViewModel* searchResults, const QString& term);
	void visit(ConfigNode& node);
	void visit(TreeViewModel* model);

private:
	TreeViewModel*	m_searchResults;
	QString			m_term;
};

#endif // FINDVISITOR_HPP
