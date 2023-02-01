WITH FUNCTION drawio_erd(include_objects_like VARCHAR2 DEFAULT NULL,
                                      exclude_objects_like VARCHAR2 DEFAULT NULL,
                                      include_csv_list     VARCHAR2 DEFAULT NULL,
                                      exclude_csv_list     VARCHAR2 DEFAULT NULL,
                                      IsCollapsed          VARCHAR2 DEFAULT NULL) RETURN CLOB  IS
  --Configuration variables
  --Please also check set_config procedure
  l_Table_Caption_Height NUMBER := 20;
  l_Table_Header_Height  NUMBER := 13;
  l_TableRowHeight       NUMBER := 17;
  l_Table_Style          VARCHAR2(1000) := 'shape=table;startSize=' || l_Table_Caption_Height ||
                                           ';container=1;collapsible=1;childLayout=tableLayout;' ||
                                           'fillColor=#f5f5f5;strokeColor=#666666;fontStyle=1;' ||
                                           'resizeLastRow=1;resizeLast=1;fontColor=#333333;rowLines=0;columnLines=0;' ||
                                           'swimlaneFillColor=default;';

  l_Header_Style VARCHAR2(1000) := 'shape=tableRow;horizontal=0;startSize=0;swimlaneHead=0;swimlaneBody=0;' ||
                                   'top=0;left=0;bottom=0;right=0;' ||
                                   'collapsible=0;dropTarget=0;fillColor=#F0F0F0;points=[[0,0.5],[1,0.5]];' ||
                                   'portConstraint=eastwest;strokeColor=none;connectable=0;';
  l_Row_Style    VARCHAR2(1000) := 'shape=tableRow;horizontal=0;startSize=0;swimlaneHead=0;swimlaneBody=0;' ||
                                   'top=0;left=0;bottom=0;right=0;' ||
                                   'collapsible=0;dropTarget=0;fillColor=none;points=[[0,0.5],[1,0.5]];' ||
                                   'portConstraint=eastwest;';

  l_arrow_style VARCHAR2(1000) := 'endArrow=classic;html=1;rounded=1;fontSize=8;' ||
                                  'startArrow=ERmany;startFill=0;edgeStyle=entityRelationEdgeStyle;' ||
                                  'labelBorderColor=#CCCCCC;fontColor=#666666;labelPosition=right;align=center;' ||
                                  'fillColor=#f5f5f5;strokeColor=#666666;';
  ------------------------------------------------------------------------------------------------
  --Types
  TYPE t_ERD_column_conf IS RECORD(
     column_name         VARCHAR2(128 CHAR)
    ,column_X            NUMBER
    ,column_Width        NUMBER
    ,header_column_style VARCHAR2(1000 CHAR)
    ,row_column_style    VARCHAR2(1000 CHAR)
    ,column_value        VARCHAR2(1000 CHAR));
  TYPE t_ERD_tab_conf IS TABLE OF t_erd_column_conf;

  TYPE t_tab_row IS RECORD(
     table_name  VARCHAR2(128 CHAR)
    ,fk          VARCHAR2(4 CHAR)
    ,uk          VARCHAR2(4 CHAR)
    ,pk          VARCHAR2(4 CHAR)
    ,column_name VARCHAR2(128 CHAR)
    ,data_type   VARCHAR2(128 CHAR)
    ,nullable    VARCHAR2(1 CHAR)
    ,comm        VARCHAR2(4000 CHAR));
  TYPE t_tab IS TABLE OF t_tab_row;

  --Local variables
  l_tab          t_tab;
  l_clob         CLOB := NULL;
  l_Table_heigth NUMBER; --current table heigth
  l_Table_Width  NUMBER; --current table width
  l_Table_Y      NUMBER; --left upper corner Y
  l_Table_X      NUMBER; --left upper corner X

  ERD_tab_conf t_ERD_tab_conf := t_ERD_tab_conf();

  ----------------------------------------------------------------------------
  --          Set ERD columns widths and styles
  ----------------------------------------------------------------------------
  PROCEDURE set_config IS
    i VARCHAR2(100);
  BEGIN
    l_Table_Y := 110;
    l_Table_X := 20;
  
    ERD_tab_conf.delete;
    ERD_tab_conf.extend;
    ERD_tab_conf(ERD_tab_conf.last) := t_ERD_column_conf(column_name         => 'fk',
                                                         column_x            => 0,
                                                         column_Width        => 20,
                                                         header_column_style => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=1;left=1;bottom=1;right=1;fontSize=9;strokeColor=#CCCCCC;',
                                                         row_column_style    => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=0;left=0;bottom=0;right=1;fontStyle=1');
    ERD_tab_conf.extend;
    ERD_tab_conf(ERD_tab_conf.last) := t_ERD_column_conf(column_name         => 'uk',
                                                         column_x            => ERD_tab_conf(ERD_tab_conf.last - 1).column_x + ERD_tab_conf(ERD_tab_conf.last - 1).column_Width,
                                                         column_Width        => 20,
                                                         header_column_style => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=1;left=1;bottom=1;right=1;fontSize=9;strokeColor=#CCCCCC;',
                                                         row_column_style    => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=0;left=1;bottom=0;right=1;fontStyle=1');
    ERD_tab_conf.extend;
    ERD_tab_conf(ERD_tab_conf.last) := t_ERD_column_conf(column_name         => 'pk',
                                                         column_x            => ERD_tab_conf(ERD_tab_conf.last - 1).column_x + ERD_tab_conf(ERD_tab_conf.last - 1).column_Width,
                                                         column_Width        => 20,
                                                         header_column_style => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=1;left=1;bottom=1;right=1;fontSize=9;strokeColor=#CCCCCC;',
                                                         row_column_style    => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=0;left=0;bottom=0;right=1;fontStyle=1');
    ERD_tab_conf.extend;
    ERD_tab_conf(ERD_tab_conf.last) := t_ERD_column_conf(column_name         => 'Column',
                                                         column_x            => ERD_tab_conf(ERD_tab_conf.last - 1).column_x + ERD_tab_conf(ERD_tab_conf.last - 1).column_Width,
                                                         column_Width        => 180,
                                                         header_column_style => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=1;left=1;bottom=1;right=1;fontSize=9;strokeColor=#CCCCCC;',
                                                         row_column_style    => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=0;left=0;bottom=0;right=1;align=left;');
    ERD_tab_conf.extend;
    ERD_tab_conf(ERD_tab_conf.last) := t_ERD_column_conf(column_name         => 'Type',
                                                         column_x            => ERD_tab_conf(ERD_tab_conf.last - 1).column_x + ERD_tab_conf(ERD_tab_conf.last - 1).column_Width,
                                                         column_Width        => 100,
                                                         header_column_style => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=1;left=1;bottom=1;right=1;fontSize=9;strokeColor=#CCCCCC;',
                                                         row_column_style    => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=0;left=0;bottom=0;right=1;align=left;');
    ERD_tab_conf.extend;
    ERD_tab_conf(ERD_tab_conf.last) := t_ERD_column_conf(column_name         => 'Null',
                                                         column_x            => ERD_tab_conf(ERD_tab_conf.last - 1).column_x + ERD_tab_conf(ERD_tab_conf.last - 1).column_Width,
                                                         column_Width        => 20,
                                                         header_column_style => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=1;left=1;bottom=1;right=1;fontSize=9;strokeColor=#CCCCCC;',
                                                         row_column_style    => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=0;left=0;bottom=0;right=1');
    ERD_tab_conf.extend;
    ERD_tab_conf(ERD_tab_conf.last) := t_ERD_column_conf(column_name         => 'Comment',
                                                         column_x            => ERD_tab_conf(ERD_tab_conf.last - 1).column_x + ERD_tab_conf(ERD_tab_conf.last - 1).column_Width,
                                                         column_Width        => 500,
                                                         header_column_style => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=1;left=1;bottom=1;right=1;fontSize=9;strokeColor=#CCCCCC;',
                                                         row_column_style    => 'shape=partialRectangle;html=1;connectable=0;overflow=hidden;fillColor=none;top=0;left=0;bottom=0;right=0;align=left;');
  
    i             := erd_tab_conf.first;
    l_Table_Width := 0;
    WHILE i IS NOT NULL LOOP
      l_Table_Width := l_Table_Width + erd_tab_conf(i).column_Width;
      i             := erd_tab_conf.next(i);
    END LOOP;
  END;
  ------------------------------------------------------------------------------------------------
  PROCEDURE add_str(In_Str VARCHAR2) IS
  BEGIN
    l_Clob := l_clob || In_Str || chr(10);
  END;
  ------------------------------------------------------------------------------------------------
  PROCEDURE add_mxCell(MxCell_id                        VARCHAR2,
                       MxCell_Value                     VARCHAR2 DEFAULT NULL,
                       MxCell_Style                     VARCHAR2,
                       MxCell_Parent                    VARCHAR2,
                       MxCell_Vertex                    NUMBER DEFAULT 1,
                       MxCell_Collapsed                 NUMBER DEFAULT NULL,
                       MxCell_Source                    VARCHAR2 DEFAULT NULL,
                       MxCell_Target                    VARCHAR2 DEFAULT NULL,
                       MxCell_Edge                      NUMBER DEFAULT NULL,
                       MxCell_MxGeometry_X              NUMBER DEFAULT NULL,
                       MxCell_MxGeometry_Y              NUMBER DEFAULT NULL,
                       MxCell_Geometry_Width            NUMBER DEFAULT NULL,
                       MxCell_Geometry_Height           NUMBER DEFAULT NULL,
                       MxCell_Geometry_Relative         NUMBER DEFAULT NULL,
                       MxCell_Geometry_Rectangle_Width  NUMBER DEFAULT NULL,
                       MxCell_Geometry_Rectangle_Height NUMBER DEFAULT NULL,
                       MxCell_MxGeometry_MxPoint_X      NUMBER DEFAULT NULL,
                       MxCell_MxGeometry_MxPoint_Y      NUMBER DEFAULT NULL) IS
    Lstr VARCHAR2(32000);
  BEGIN
    --possible solution in case of encodinig issue:
    --dbms_xmlgen.convert(l_tab(rowidx).comm, dbms_xmlgen.ENTITY_encode);
    SELECT XMLSerialize(CONTENT(XMLELEMENT("mxCell",
                                            XMLATTRIBUTES(MxCell_id AS "id",
                                                          MxCell_Value AS "value",
                                                          MxCell_Style AS "style",
                                                          MxCell_Parent AS "parent",
                                                          MxCell_Vertex AS "vertex",
                                                          MxCell_collapsed AS "collapsed",
                                                          MxCell_Source AS "source",
                                                          MxCell_Target AS "target",
                                                          MxCell_Edge AS "edge"),
                                            XMLELEMENT("mxGeometry",
                                                       XMLATTRIBUTES(MxCell_MxGeometry_X AS "x",
                                                                     MxCell_MxGeometry_Y AS "y",
                                                                     MxCell_Geometry_Width AS "width",
                                                                     MxCell_Geometry_Height AS "height",
                                                                     MxCell_Geometry_Relative AS "relative",
                                                                     'geometry' AS "as"),
                                                       CASE
                                                         WHEN MxCell_Geometry_Rectangle_Width IS NOT NULL
                                                              OR MxCell_Geometry_Rectangle_Height IS NOT NULL THEN
                                                          XMLELEMENT("mxRectangle",
                                                                     XMLATTRIBUTES(MxCell_Geometry_Rectangle_Width AS "width",
                                                                                   MxCell_Geometry_Rectangle_Height AS "height",
                                                                                   'alternateBounds' AS "as"))
                                                       END,
                                                       CASE
                                                         WHEN MxCell_MxGeometry_MxPoint_X IS NOT NULL
                                                              AND MxCell_MxGeometry_MxPoint_Y IS NOT NULL
                                                              AND MxCell_Style LIKE '%entityRelationEdgeStyle%' THEN
                                                          XMLELEMENT("mxPoint",
                                                                     XMLATTRIBUTES(MxCell_MxGeometry_MxPoint_X AS "x",
                                                                                   MxCell_MxGeometry_MxPoint_Y AS "y",
                                                                                   'targetPoint' AS "as"))
                                                       END
                                                       ---
                                                       )))) AS mcCell
    INTO   Lstr
    FROM   dual;
    add_str(In_Str => Lstr);
  END;
  -----------------------------------------------------------------
  PROCEDURE add_table_header(InTableName VARCHAR2,
                             InRowsCount NUMBER,
                             IsCollapsed NUMBER) IS
  BEGIN
    l_Table_heigth := (l_Table_Caption_Height + l_Table_Header_Height) + InRowsCount * l_TableRowHeight;
    --table declaration
    IF IsCollapsed IS NOT NULL
    THEN
      add_mxCell(MxCell_id                        => InTableName,
                 MxCell_Value                     => InTableName,
                 MxCell_Style                     => l_Table_Style,
                 MxCell_Parent                    => 1,
                 MxCell_Vertex                    => 1,
                 MxCell_Collapsed                 => 1,
                 MxCell_MxGeometry_X              => l_Table_X,
                 MxCell_MxGeometry_Y              => l_Table_Y,
                 MxCell_Geometry_Width            => l_Table_Width,
                 MxCell_Geometry_Height           => 30,
                 MxCell_Geometry_Rectangle_Height => l_Table_heigth,
                 MxCell_Geometry_Rectangle_Width  => l_Table_Width);
    ELSE
      add_mxCell(MxCell_id              => InTableName,
                 MxCell_Value           => InTableName,
                 MxCell_Style           => l_Table_Style,
                 MxCell_Parent          => 1,
                 MxCell_Vertex          => 1,
                 MxCell_MxGeometry_X    => l_Table_X,
                 MxCell_MxGeometry_Y    => l_Table_Y,
                 MxCell_Geometry_Width  => l_Table_Width,
                 MxCell_Geometry_Height => l_Table_heigth);
    END IF;
  
    --table  header
    add_mxCell(MxCell_id              => InTableName || '-HEADER',
               MxCell_Style           => l_Header_Style,
               MxCell_Parent          => InTableName,
               MxCell_Vertex          => 1,
               MxCell_MxGeometry_Y    => l_Table_Caption_Height,
               MxCell_Geometry_Width  => l_Table_Width,
               MxCell_Geometry_Height => l_Table_Header_Height);
    --table heasder cells
    DECLARE
      i NUMBER;
    BEGIN
      i := erd_tab_conf.first;
      WHILE i IS NOT NULL LOOP
        dbms_output.put_line(i || '.' || erd_tab_conf(i).column_name);
        add_mxCell(MxCell_id                        => InTableName || '-HEADER-' || upper(erd_tab_conf(i).column_name),
                   MxCell_Value                     => erd_tab_conf(i).column_name,
                   MxCell_Style                     => erd_tab_conf(i).header_column_style,
                   MxCell_Parent                    => InTableName || '-HEADER',
                   MxCell_Vertex                    => 1,
                   MxCell_MxGeometry_X              => erd_tab_conf(i).column_x,
                   MxCell_Geometry_Width            => erd_tab_conf(i).column_Width,
                   MxCell_Geometry_Height           => l_Table_Header_Height,
                   MxCell_Geometry_Rectangle_Width  => erd_tab_conf(i).column_Width,
                   MxCell_Geometry_Rectangle_Height => l_Table_Header_Height);
        i := erd_tab_conf.next(i);
      END LOOP;
    END;
  
  END;
  --------------------------------------------------------------------
  FUNCTION get_Column_references_text(IntableName  VARCHAR2,
                                      InColumnName VARCHAR2) RETURN VARCHAR2 IS
    l_keys VARCHAR2(1000);
  BEGIN
    SELECT listagg(refcols, ',') WITHIN GROUP(ORDER BY refcols) comm
    INTO   l_keys
    FROM   (SELECT DISTINCT cons.cons_columns || '->' || refcons.table_name || '(' || listagg(refcons.column_name, ',') WITHIN GROUP(ORDER BY refcons.position) OVER(PARTITION BY refcons.table_name, refcons.constraint_name) || ') ' refcols
            FROM   (SELECT DISTINCT listagg(t1.column_name, ',') WITHIN GROUP(ORDER BY t1.position) OVER(PARTITION BY t1.table_name, t1.constraint_name) cons_columns
                                   , MAX(CASE
                                           WHEN t1.column_name = InColumnName THEN
                                            'YES'
                                           ELSE
                                            'NO'
                                         END) OVER(PARTITION BY t1.table_name, t1.constraint_name) AS is_exists
                                   ,user_constraints.r_constraint_name
                    FROM   user_cons_columns t1
                    JOIN   user_constraints ON t1.table_name = user_constraints.table_name
                                               AND t1.constraint_name = user_constraints.constraint_name
                                               AND CONSTRAINT_type = 'R'
                    WHERE  t1.table_name = IntableName) cons
            JOIN   user_cons_columns refcons ON cons.r_constraint_name = refcons.constraint_name
            WHERE  is_exists = 'YES');
    RETURN l_keys;
  END;
  --------------------------------------------------------------------
  FUNCTION get_Link_references_text(IntableName  VARCHAR2,
                                    InColumnName VARCHAR2) RETURN VARCHAR2 IS
    --TODO Sometime returns many references in case two columns are incluses in reference
    l_keys VARCHAR2(1000);
  BEGIN
    SELECT listagg(refcols, ',') WITHIN GROUP(ORDER BY refcols) comm
    INTO   l_keys
    FROM   (SELECT DISTINCT cons.cons_columns || '->' || chr(13) || chr(10) || refcons.table_name || '(' ||
                            listagg(refcons.column_name, ',') WITHIN GROUP(ORDER BY refcons.position) OVER(PARTITION BY refcons.table_name, refcons.constraint_name) || ') ' refcols
            FROM   (SELECT DISTINCT listagg(t1.table_name || '(' || t1.column_name || ')', ',') WITHIN GROUP(ORDER BY t1.position) OVER(PARTITION BY t1.table_name, t1.constraint_name) cons_columns
                                   , MAX(CASE
                                           WHEN t1.column_name = InColumnName THEN
                                            'YES'
                                           ELSE
                                            'NO'
                                         END) OVER(PARTITION BY t1.table_name, t1.constraint_name) AS is_exists
                                   ,user_constraints.r_constraint_name
                    FROM   user_cons_columns t1
                    JOIN   user_constraints ON t1.table_name = user_constraints.table_name
                                               AND t1.constraint_name = user_constraints.constraint_name
                                               AND CONSTRAINT_type = 'R'
                    WHERE  t1.table_name = IntableName) cons
            JOIN   user_cons_columns refcons ON cons.r_constraint_name = refcons.constraint_name
            WHERE  is_exists = 'YES');
    RETURN l_keys;
  END;
  ---------------------------------------------------------------------------------------------------------
  PROCEDURE add_table_references(InTableName VARCHAR2) IS
  BEGIN
    FOR k IN (SELECT src.constraint_name
                    ,src.table_name
                    ,src.column_name
                    ,src.position
                    ,user_constraints.r_constraint_name target_constraint_name
                    ,trg.table_name                     target_table_name
                    ,trg.column_name                    target_column_name
              FROM   user_cons_columns src
              JOIN   user_constraints ON src.table_name = user_constraints.table_name
                                         AND src.constraint_name = user_constraints.constraint_name
                                         AND user_constraints.CONSTRAINT_type = 'R'
              JOIN   user_cons_columns trg ON src.owner = trg.owner
                                              AND user_constraints.r_constraint_name = trg.constraint_name
                                              AND src.position = trg.position
              WHERE  src.table_name = InTableName
              ORDER  BY src.table_name, src.constraint_name, src.position) LOOP
      add_mxCell(MxCell_id                   => k.constraint_name || '_' || k.position,
                 MxCell_Value                => get_Link_references_text(InTableName  => k.table_name,
                                                                         InColumnName => k.Column_name), -- k.constraint_name,
                 MxCell_Style                => l_arrow_style,
                 MxCell_Parent               => 1,
                 MxCell_Vertex               => NULL,
                 MxCell_Source               => k.table_name || '-' || k.column_name,
                 MxCell_Target               => k.target_table_name || '-' || k.target_column_name,
                 MxCell_MxGeometry_X         => -0.2,
                 MxCell_Edge                 => 1,
                 MxCell_Geometry_Relative    => 1,
                 MxCell_Geometry_Width       => 50,
                 MxCell_Geometry_Height      => 50,
                 MxCell_MxGeometry_MxPoint_X => l_Table_X + l_Table_Width + 100,
                 MxCell_MxGeometry_MxPoint_Y => l_Table_Y - 20);
    END LOOP;
  END;
  ---------------------------------------------------------------------------------------------------------
  PROCEDURE add_document_header IS
  BEGIN
    add_str(In_Str => '<mxGraphModel dx="1120" dy="754" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1"');
    add_str(In_Str => '              fold="1" page="1" pageScale="1" pageWidth="850" pageHeight="1100" math="0" shadow="0">');
    add_str(In_Str => '<root>');
    add_str(In_Str => '  <mxCell id="0" />');
    add_str(In_Str => '  <mxCell id="1" parent="0" />');
  END;
  ---------------------------------------------------------------------------------------------------------
  PROCEDURE add_document_footer IS
  BEGIN
    add_str(In_Str => '</root>');
    add_str(In_Str => '</mxGraphModel>');
  END;
  --#######################################################################################################################
  --                                  Main function
  --#######################################################################################################################
  /*
  (include_objects_like VARCHAR2 DEFAULT NULL,
  exclude_objects_like VARCHAR2 DEFAULT NULL,
  include_csv_list     VARCHAR2 DEFAULT NULL,
  exclude_csv_list     VARCHAR2 DEFAULT NULL,
  IsCollapsed          VARCHAR2 DEFAULT NULL) RETURN CLOB
  */
BEGIN
  L_clob := NULL;
  set_config;
  add_document_header;

  FOR global_tab IN (SELECT table_name
                     FROM   user_tables
                     WHERE  (include_objects_like IS NULL OR user_tables.table_name LIKE include_objects_like ESCAPE '\')
                            AND
                            (exclude_objects_like IS NULL OR user_tables.table_name NOT LIKE exclude_objects_like ESCAPE '\')
                            AND (include_csv_list IS NULL OR
                            user_tables.table_name IN
                            (SELECT regexp_substr(include_csv_list, '[^,]+', 1, LEVEL) AS r
                                  FROM   dual
                                  CONNECT BY regexp_substr(include_csv_list, '[^,]+', 1, LEVEL) IS NOT NULL))
                            AND (exclude_csv_list IS NULL OR
                            user_tables.table_name NOT IN
                            (SELECT regexp_substr(exclude_csv_list, '[^,]+', 1, LEVEL) AS r
                                  FROM   dual
                                  CONNECT BY regexp_substr(exclude_csv_list, '[^,]+', 1, LEVEL) IS NOT NULL))
                     ORDER  BY /*(SELECT COUNT(*)
                                                                                                                                      FROM   user_cons_columns
                                                                                                                                      WHERE  user_cons_columns.table_name = user_tables.table_name) DESC*/
                               table_name) LOOP
  
    SELECT user_tab_cols.table_name
          ,cons_FK.constraint_type FK
          ,cons_UK.constraint_type UK
          ,cons_pK.constraint_type PK
          ,user_tab_cols.column_name
          , user_tab_cols.data_type || CASE
              WHEN user_tab_cols.char_length <> 0 THEN
               '(' || user_tab_cols.char_length || ' ' || CASE
                 WHEN user_tab_cols.char_used = 'C' THEN
                  'CHAR'
                 ELSE
                  'BYTE'
               END || ')'
              WHEN user_tab_cols.data_precision IS NULL THEN
               '(' || to_char(user_tab_cols.data_length) || ')'
              ELSE
               '(' || to_char(user_tab_cols.data_precision) || ',' || to_char(user_tab_cols.data_scale) || ')'
            END data_type
          , CASE
              WHEN user_tab_cols.nullable = 'Y' THEN
               'Y'
              ELSE
               NULL
            END nullable
          ,user_col_comments.comments
    BULK   COLLECT
    INTO   l_tab
    FROM   user_tab_cols
    LEFT   JOIN user_col_comments ON user_tab_cols.table_name = user_col_comments.table_name
                                     AND user_tab_cols.column_name = user_col_comments.column_name
    LEFT   JOIN (SELECT DISTINCT user_cons_columns.table_name
                                ,user_cons_columns.column_name
                                ,user_constraints.CONSTRAINT_type
                 FROM   user_cons_columns
                 JOIN   user_constraints ON user_cons_columns.table_name = user_constraints.table_name
                                            AND user_cons_columns.constraint_name = user_constraints.constraint_name
                                            AND CONSTRAINT_type = 'P') cons_pK ON cons_pK.table_name =
                                                                                  user_tab_cols.table_name
                                                                                  AND cons_pK.column_name =
                                                                                  user_tab_cols.column_name
    LEFT   JOIN (SELECT DISTINCT user_cons_columns.table_name
                                ,user_cons_columns.column_name
                                ,user_constraints.CONSTRAINT_type
                 FROM   user_cons_columns
                 JOIN   user_constraints ON user_cons_columns.table_name = user_constraints.table_name
                                            AND user_cons_columns.constraint_name = user_constraints.constraint_name
                                            AND CONSTRAINT_type = 'U') cons_UK ON cons_UK.table_name =
                                                                                  user_tab_cols.table_name
                                                                                  AND cons_UK.column_name =
                                                                                  user_tab_cols.column_name
    LEFT   JOIN (SELECT DISTINCT user_cons_columns.table_name
                                ,user_cons_columns.column_name
                                ,user_constraints.CONSTRAINT_type
                 FROM   user_cons_columns
                 JOIN   user_constraints ON user_cons_columns.table_name = user_constraints.table_name
                                            AND user_cons_columns.constraint_name = user_constraints.constraint_name
                                            AND CONSTRAINT_type = 'R') cons_FK ON cons_FK.table_name =
                                                                                  user_tab_cols.table_name
                                                                                  AND cons_FK.column_name =
                                                                                  user_tab_cols.column_name
    WHERE  user_tab_cols.table_name = global_tab.table_name
           AND user_tab_cols.column_name NOT IN ('STATUS', 'CREATED_ON', 'CREATED_BY', 'UPDATED_ON', 'UPDATED_BY')
    ORDER  BY user_tab_cols.table_name, PK, UK, FK, user_tab_cols.column_NAME;
    -----------
    add_table_header(InTableName => global_tab.table_name, InRowsCount => l_tab.count, IsCollapsed => IsCollapsed);
    -----------
  
    DECLARE
      rowidx NUMBER;
    BEGIN
      --loop by table rows
      rowidx := l_tab.first;
      WHILE rowidx IS NOT NULL LOOP
        --Row declaration
        add_mxCell(MxCell_id              => l_tab(rowidx).Table_Name || '-' || l_tab(rowidx).column_name,
                   MxCell_Style           => l_Row_Style,
                   MxCell_Parent          => l_tab(rowidx).Table_Name,
                   MxCell_Vertex          => 1,
                   MxCell_MxGeometry_Y    => l_Table_Caption_Height + l_Table_Header_Height +
                                             l_TableRowHeight * (rowidx - 1),
                   MxCell_Geometry_Width  => l_Table_Width,
                   MxCell_Geometry_Height => l_TableRowHeight);
      
        erd_tab_conf(1).column_value := l_tab(rowidx).fk;
        erd_tab_conf(2).column_value := l_tab(rowidx).uk;
        erd_tab_conf(3).column_value := l_tab(rowidx).pk;
        erd_tab_conf(4).column_value := l_tab(rowidx).column_name;
        erd_tab_conf(5).column_value := l_tab(rowidx).data_type;
        erd_tab_conf(6).column_value := l_tab(rowidx).nullable;
        erd_tab_conf(7).column_value := nvl(get_Column_references_text(InTableName  => l_tab(rowidx).table_name,
                                                                       InColumnName => l_tab(rowidx).Column_name),
                                            l_tab(rowidx).comm);
      
        --loop by columns definition to add columns
        DECLARE
          erdidx NUMBER;
        BEGIN
          erdidx := erd_tab_conf.first;
          WHILE erdidx IS NOT NULL LOOP
            add_mxCell(MxCell_id                        => l_tab(rowidx).Table_Name || '-' || l_tab(rowidx).column_name || '-' ||
                                                            upper(erd_tab_conf(erdidx).column_name),
                       MxCell_Value                     => erd_tab_conf(erdidx).column_value,
                       MxCell_Style                     => erd_tab_conf(erdidx).row_column_style,
                       MxCell_Parent                    => l_tab(rowidx).Table_Name || '-' || l_tab(rowidx).column_name,
                       MxCell_Vertex                    => 1,
                       MxCell_MxGeometry_X              => erd_tab_conf(erdidx).column_x,
                       MxCell_Geometry_Width            => erd_tab_conf(erdidx).column_Width,
                       MxCell_Geometry_Height           => l_TableRowHeight,
                       MxCell_Geometry_Rectangle_Width  => erd_tab_conf(erdidx).column_Width,
                       MxCell_Geometry_Rectangle_Height => l_TableRowHeight);
            erdidx := erd_tab_conf.next(erdidx);
          END LOOP;
        END;
        rowidx := l_tab.next(rowidx);
      END LOOP;
    END;
    add_table_references(InTableName => global_tab.table_name);
    --shift next table position
    IF IsCollapsed IS NOT NULL
    THEN
      l_Table_Y := l_Table_Y + l_Table_Caption_Height + 20;
    ELSE
      l_Table_Y := l_Table_Y + l_Table_heigth + 20;
    END IF;
    IF l_Table_Y > 1000
    THEN
      l_Table_Y := 110;
      l_Table_X := l_Table_X + l_Table_Width + 150;
    END IF;
  
  END LOOP;

  add_document_footer;
  RETURN l_clob;
END;
SELECT drawio_erd(include_objects_like => '%ACTION%') FROM dual
