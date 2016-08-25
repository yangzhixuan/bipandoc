Docx Summary
----

This is a summary for the work about docx format of two months.


### 1. Docx Format
   1. Overview
	
     Actually, a `docx` file is a `zip` package containing many parts of the document, mostly XML files.
	  
	  The files in the package are organized as a tree structure. A simple one is like this:
	   
  		[Content_Types].xml  _rels  docProps  word
  		
  		./_rels:
  		./docProps:
  			app.xml  core.xml  custom.xml
		./word:
			_rels  theme  document.xml	 fontTable.xml  settings.xml  styles.xml
		./word/_rels:
			document.xml.rels
		./word/theme:
			theme1.xml
			
   Most text information is in the `document.xml` file. It is the most important file in `docx` package and it shows the structure of the document.
   
   2. Block Element
	 
	 * Paragraph
	 
	     Paragraph is the basic unit in `docx` document. It shows like this:
	     
				<w:p>
                    <w:pPr>
                        <w:pStyle w:val="FirstParagraph"/>
                    </w:pPr>
                    <w:r>
                        <w:rPr>
                            <w:rStyle w:val=“Normal”/>
                            <w:i/>
                        </w:rPr>
                        <w:t xml:space=“preserve">simple test</w:t>
                    </w:r>
                </w:p>

	 		`<w:p>` means this is a paragraph, `<w:pPr>` contains the properties of this paragraph, `<w:r>` contains the content of this paragraph. However, there may be many `<w:r>` in one paragraph.
	 	
	 * Table

	 A table consists of rows and cells and is structured much like an `HTML` table. It is defined with the `<w:tbl>` element. A simple example is like this:
	 
	 		<w:tbl>
				<w:tblPr>
					<w:tblStyle w:val="TableGrid"/>
					<w:tblW w:w="5000" w:type="pct"/>
				</w:tblPr>
				<w:tblGrid>
					<w:gridCol w:w="2880"/>
					<w:gridCol w:w="2880"/>
					<w:gridCol w:w="2880"/>
				</w:tblGrid>
				<w:tr>
					<w:tc>
						<w:tcPr>
							<w:tcW w:w="2880" w:type="dxa"/>
						</w:tcPr>
						<w:p>
							<w:r>
								<w:t>AAA</w:t>
							</w:r>
						</w:p>
					</w:tc>
					. . .					
				</w:tr>
				. . .
			</w:tbl>
			
			
		`<w:tblGrid>` specifies the columns for the table.
		`<tblPr>` specifies the table-wide properties for the table. 
	 	`<w:tr>` specifies a table row.
	 
	 * List
	 
	     Actually, a list item is just a paragraph, but in `<w:pPr>`, there is a tag `<w:numPr>` to show its information of list level and list style. Like this:
	     
	     	<w:numPr>
				<w:ilvl w:val="1"/> (list level)
          		<w:numId w:val="1"/> (list style reference)
       		</w:numPr>
		So, that means, in `docx` format, list structure is not nested, and every list item is parallel to others. 

   3. Inline Element
	
	 * **bold**/*italic*

		In `docx` document, these style attributes are in the `<w:rPr>` tag. Like this:
				
			<w:r>
         		<w:rPr>
					<w:b/>
					<w:i/>
             		. . .
             	</w:rPr>
             	<w:t>test</w:t>
         	</w:r>
		It will show like this: ***test***
	 
	 * link
	 	
	 	Link is showed with tag `<w:hyperlink>`, like this:
	 		
	 		<w:hyperlink r:id="rId2">
     			<w:r>
         			<w:rPr>
						...
             		</w:rPr>
             		<w:t>Google</w:t>
         		</w:r>
    		</w:hyperlink>
    	
    	`r:id` is a reference to the link target URL which is stored in another file in `docx` package. 

	
   4. Helpful Tool and Website 
   		
   	The parts above just show some basic elements in `docx` format. You can find more details by helpful information below:
   
	* online parser (written by Java): [link](http://webapp.docx4java.org/OnlineDemo/PartsList.html)

		This is a online tool to take apart the `docx` file. You can see the structure and information in every file in the structure.

	* format document: [link](http://officeopenxml.com/WPcontentOverview.php)

		This is the document of the `docx` format. It contains all the details.



### 2. Docx Reader

There is a reader for `docx` now. It can convert the `docx` document to the AST of BiPandoc. However, it just preserves the text information that AST supports and drops the style information and what AST doesn't support now (e.g. table).

The reader is the `BiPandoc/Reader/DocxReader.hs` file. And the test code is the `BiPandoc/tests/TestDocxReader.hs`. 

You can easy to test it when load the test program and run
 			
 	ghci> testDocxReader "docx file name"

**Hint:** The file path of the docx file must be the same as where the ghci is running. And the double quotation marks are necessary.







### 3. Problems about BX of Docx

When I try to do synchronization between AST and docx, I find some problems hard to resolve in BiGUL.

1. Separated Information

	As I mentioned above, the information of link and list is separated.
	
	* Separation of target and text in link

		For link, you have to use the `r:id` value to find the target URL in the file `word/_rels/document.xml.rels`, and the code like this: 
			
			<Relationship Id="rId2" Type=“…” Target="http://www.google.com/" TargetMode=“External"/>
		
	* Separation of style and content in list
		
		For list, you have to use the value of `<w:numId>` and `<w:ilvl>` to find the style info in the file `word/numbering.xml`.
	
		In this file, you have to find the `abstractNumId` by `numId` firstly, and then use the `abstractNumId` and the `ilvl` value to find the style info. An example is like this:
	
			<w:num w:numId="1">
	    		<w:abstractNumId w:val="1"/>
			</w:num>
	--
			<w:abstractNum w:abstractNumId="1">
	 			<w:lvl w:ilvl="0">
	     			<w:start w:val="1"/>
	         		<w:numFmt w:val="decimal"/>
	         		...
	      		</w:lvl>
	      		...
			</w:abstractNum>

		In the example, we can find the list item is of the 0 level (outermost) and the style is **decimal**. That means it is an ordered list item and the order style is decimal. If unordered list item, the style will be **bullet**.
		
	* The essence of problem

		Because of the separation, when we need to do `put` operation, we need to update two files separately. Abstractly, we can consider the `document.xml` file as the main structure, and the other files as the global variable, the problem is that when we do sync between AST and the main structure, we need to update the global variable sometimes.
	
		
	
2. Tree-path Problem

	In a paragraph, if the attributes of the text is different, the text will be divided into different `<w:r>`. Here is an example:
	
		<w:p>
			<w:pPr>
				...
			</w:pPr>
			<w:r>
          		<w:rPr>
             		<w:b/> <w:bCs/>
          		</w:rPr>
                <w:t>a</w:t>
	     	</w:r>
        	<w:r>
          		<w:rPr>
                	<w:b/> <w:bCs/>
                	<w:i/> <w:iCs/>
            	</w:rPr>
            	<w:t>b</w:t>
        	</w:r>
        </w:p>

	It is the code of text: **a*****b***  (a,b are bold; b is italic)
	
	But in AST, the bold attribute is shared, like this: 
	
		[bold [str a, italic [str b]]]
		
	That means, the list of `<w:r>` is the list of all paths to the leaf in the structure tree of AST. 


### 4. Future Work

We need to find a way to solve the two problems mentioned above. And then BX may be doable. Otherwise, we may need to implement a temporary one by unidirectional program language.