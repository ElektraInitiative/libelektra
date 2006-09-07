<t:templates xmlns:t="http://nwalsh.com/docbook/xsl/template/1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	base-stylesheet="html-params.xsl">

	<t:titlepage element="article" wrapper="div" class="titlepage">
		<t:titlepage-content side="recto">
			<title predicate="[1]"/>
			<pubdate/>

			<authorgroup>
				<t:or>
					<author>
						<t:or>
							<firstname/>
							<affiliation>
								<t:or>
									<address/>
									<orgdiv/>
									<orgname/>
								</t:or>
							</affiliation>
						</t:or>
					</author>
				</t:or>
			</authorgroup>

		</t:titlepage-content>
		
		<t:titlepage-content side="verso"/>
 
	</t:titlepage>
</t:templates>
