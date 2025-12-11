:- use_module(library(lists)).
:- use_module(library(aggregate)).


% Knowledge Graph we built from the internet 

related('azure', '.net', 0.01).
related('sql-server', '.net', 0.12).
related('asp.net', '.net', 0.97).
related('entity-framework', '.net', 0.04).
related('wpf', '.net', 0.12).
related('linq', '.net', 0.00).
related('wcf', '.net', 0.08).
related('c#', '.net', 0.8).

related('tdd', 'agile', 0.16).
related('codeigniter', 'ajax', 0.03).
related('javascript', 'ajax', 0.08).
related('jquery', 'ajax', 0.22).
related('php', 'ajax', 0.25).
related('xml', 'ajax', 0.09).
related('asp.net', 'ajax', 0.11).
related('json', 'ajax', 0.14).
related('javascript', 'algorithm', 0.02).
related('jquery', 'algorithm', 0.02).
related('math', 'algorithm', 0.18).
related('c#', 'algorithm', 0.10).
related('java', 'algorithm', 0.09).
related('python', 'algorithm', 0.09).
related('linq', 'algorithm', 0.00).
related('wpf', 'animation', 0.06).
related('flash', 'animation', 0.18).
related('css', 'animation', 0.09).
related('javascript', 'animation', 0.07).
related('jquery', 'animation', 0.14).
related('winforms', 'animation', 0.08).
related('c++', 'arrays', 0.08).
related('c', 'arrays', 0.10).
related('php', 'arrays', 0.09).
related('ruby', 'arrays', 0.05).
related('python', 'arrays', 0.15).
related('javascript', 'arrays', 0.12).
related('objective-c', 'block', 0.07).
related('c#', 'casting', 0.04).
related('java', 'casting', 0.03).
related('c++', 'casting', 0.07).
related('objective-c', 'categories', 0.06).
related('asp.net-mvc', 'controller', 0.08).
related('mysql', 'database', 0.20).
related('sql', 'database', 0.19).
related('sql-server', 'database', 0.22).
related('php', 'database', 0.17).
related('linq-to-sql', 'database', 0.13).
related('oracle', 'database', 0.18).
related('sqlite', 'database', 0.13).
related('hibernate', 'database', 0.14).
related('ruby-on-rails', 'database', 0.13).
related('asp.net-mvc', 'database', 0.10).
related('codeigniter', 'database', 0.08).
related('linq-to-entities', 'database', 0.17).
related('nhibernate', 'database', 0.14).
related('ssi', 'database', 0.39).
related('jquery', 'date', 0.04).
related('datetime', 'date', 0.04).
related('mysql', 'date', 0.08).
related('sql', 'date', 0.08).
related('c#', 'date', 0.07).
related('php', 'date', 0.09).
related('javascript', 'date', 0.07).
related('asp.net', 'date', 0.09).
related('oracle', 'date', 0.06).
related('wpf', 'datagrid', 0.08).
related('linq', 'datagrid', 0.04).
related('entity-framework', 'datagrid', 0.07).
related('wpf', 'datatemplate', 0.09).
related('wpf', 'dependency-properties', 0.10).
related('asp.net-mvc', 'entity-framework', 0.09).
related('asp.net', 'entity-framework', 0.06).
related('linq', 'entity-framework', 0.11).
related('unit-testing', 'entity-framework', 0.09).
related('linq-to-entities', 'entity-framework', 0.11).
related('sql-server', 'entity-framework', 0.11).
related('sql', 'entity-framework', 0.08).
related('asp.net-mvc-3', 'entity-framework', 0.10).
related('refactoring', 'entity-framework', 0.05).
related('validation', 'entity-framework', 0.08).
related('wpf', 'events', 0.05).
related('oop', 'events', 0.00).
related('jquery', 'events', 0.05).
related('javascript', 'events', 0.05).
related('iphone', 'facebook', 0.13).
related('javascript', 'facebook', 0.03).
related('jquery', 'facebook', 0.03).
related('f#', 'functional-programming', 0.13).
related('lisp', 'functional-programming', 0.10).
related('haskell', 'functional-programming', 0.09).
related('linq', 'functional-programming', 0.01).
related('ocaml', 'functional-programming', 0.13).
related('scala', 'functional-programming', 0.13).
related('c#', 'functional-programming', 0.05).
related('machine-learning', 'graph', 0.17).
related('wpf', 'graphics', 0.09).
related('silverlight', 'graphics', 0.14).
related('opengl', 'graphics', 0.13).
related('wpf', 'grid', 0.04).
related('jquery-ui', 'grid', 0.08).
related('css', 'grid', 0.01).
related('wpf', 'grid-layout', 0.11).
related('wpf', 'hierarchicaldatatemplate', 0.09).
related('c++', 'inheritance', 0.08).
related('c#', 'inheritance', 0.04).
related('java', 'inheritance', 0.04).
related('c', 'inline', 0.03).
related('jquery', 'input', 0.01).
related('wpf', 'itemscontrol', 0.09).
related('wpf', 'layout', 0.10).
related('swing', 'layout-manager', 0.06).
related('jquery', 'lightbox', 0.07).
related('javascript', 'lightbox', 0.03).
related('wpf', 'linq', 0.00).
related('wcf', 'linq', 0.06).
related('entity-framework', 'linq', 0.11).
related('linq-to-xml', 'linq', 0.08).
related('.net', 'linq', 0.00).
related('linq-to-sql', 'linq', 0.10).
related('c#', 'linq', 0.12).
related('sql', 'linq', 0.03).
related('linq-to-entities', 'linq', 0.12).
related('visual-studio-2010', 'linq', 0.03).
related('winforms', 'linq', 0.03).
related('lambda', 'linq', 0.04).
related('linq-to-sql', 'linq-to-entities', 0.10).
related('entity-framework', 'linq-to-entities', 0.11).
related('linq', 'linq-to-xml', 0.08).
related('silverlight', 'listbox', 0.10).
related('wpf', 'listbox', 0.09).
related('wpf', 'listview', 0.09).
related('wpf', 'mvvm', 0.17).
related('wpf', 'nhibernate', 0.04).
related('jsf', 'primefaces', 0.08).
related('asp.net', 'razor', 0.04).
related('asp.net-mvc-3', 'razor', 0.14).
related('asp.net-mvc', 'razor', 0.11).
related('validation', 'razor', 0.00).
related('wcf', 'rest', 0.07).
related('iphone', 'sdk', 0.09).
related('asp.net-mvc', 'session', 0.02).
related('asp.net', 'session', 0.05).
related('objective-c', 'singleton', 0.06).
related('wcf', 'soap', 0.07).
related('ajax', 'spring-mvc', 0.04).
related('java', 'spring-mvc', 0.06).
related('jquery', 'spring-mvc', 0.07).
related('javascript', 'spring-mvc', 0.02).
related('wpf', 'styles', 0.15).
related('css', 'stylesheet', 0.02).
related('wpf', 'templates', 0.09).
related('wpf', 'toolkit', 0.09).
related('ruby-on-rails', 'validations', 0.10).
related('asp.net', 'validation', 0.10).
related('jquery', 'validation', 0.04).
related('entity-framework', 'validation', 0.08).
related('razor', 'validation', 0.00).
related('ruby-on-rails', 'view', 0.14).
related('wpf', 'viewmodel', 0.11).
related('asp.net-mvc', 'viewmodel', 0.06).
related('asp.net-mvc', 'views', 0.10).
related('iphone', 'xcode', 0.14).
related('objective-c', 'xcode', 0.13).
related('eclipse', 'xdebug', 0.03).
related('php', 'xdebug', 0.06).
related('wpf', 'xaml', 0.12).
related('c#', 'xaml', 0.16).
related('wpf', 'xaml', 0.12).
related('wpf', '.net', 0.12).
related('asp.net-mvc', '.net', 0.19).
related('visual-studio', '.net', 0.10).
related('winforms', '.net', 0.07).
related('wcf', '.net', 0.08).
related('vb.net', '.net', 0.06).
related('iis', '.net', 0.02).
related('sql-server-2008', '.net', 0.02).
related('linq', '.net', 0.00).
related('entity-framework', '.net', 0.04).
related('visual-studio-2010', '.net', 0.05).
related('asp.net', '.net', 0.27).
related('sql-server', '.net', 0.12).
related('visual-studio-2008', '.net', 0.02).
related('web-services', '.net', 0.03).
related('sql', '.net', 0.03).
related('t-sql', '.net', 0.03).
related('sql-server-2005', '.net', 0.01).
related('linq-to-sql', '.net', 0.03).
related('asp.net-mvc-3', '.net', 0.07).
related('asp.net-mvc-2', '.net', 0.03).
related('ado.net', '.net', 0.03).
related('silverlight', '.net', 0.03).
related('c#-4.0', '.net', 0.05).
related('visual-studio-2012', '.net', 0.03).
related('azure', '.net', 0.01).

related('asp.net-mvc', 'asp.net', 0.58).
related('asp.net-mvc-2', 'asp.net', 0.17).
related('asp.net-mvc-3', 'asp.net', 0.32).
related('vb.net', 'asp.net', 0.06).
related('linq', 'asp.net', 0.18).
related('sql-server', 'asp.net', 0.27).
related('mvc', 'asp.net', 0.11).
related('webforms', 'asp.net', 0.15).
related('iis', 'asp.net', 0.09).
related('validation', 'asp.net', 0.10).
related('javascript', 'asp.net', 0.11).
related('c#', 'asp.net', 0.62).
related('ajax', 'asp.net', 0.11).
related('sql', 'asp.net', 0.15).
related('.net', 'asp.net', 0.27).
related('jquery', 'asp.net', 0.40).
related('html', 'asp.net', 0.08).
related('web-services', 'asp.net', 0.12).
related('session', 'asp.net', 0.05).
related('datetime', 'asp.net', 0.09).
related('security', 'asp.net', 0.07).
related('wcf', 'asp.net', 0.16).
related('asp.net-mvc', 'asp.net-mvc-2', 0.20).
related('asp.net-mvc-3', 'asp.net-mvc-2', 0.11).
related('c#', 'asp.net-mvc-2', 0.11).
related('asp.net', 'asp.net-mvc-2', 0.17).
related('.net', 'asp.net-mvc-2', 0.03).
related('jquery', 'asp.net-mvc-2', 0.04).
related('asp.net', 'asp.net-mvc-3', 0.32).
related('asp.net-mvc', 'asp.net-mvc-3', 0.32).
related('c#', 'asp.net-mvc-3', 0.16).
related('.net', 'asp.net-mvc-3', 0.07).
related('jquery', 'asp.net-mvc-3', 0.16).
related('razor', 'asp.net-mvc-3', 0.14).
related('javascript', 'asp.net-mvc-3', 0.02).
related('asp.net-mvc', 'asp.net-mvc-4', 0.08).
related('c#', 'asp.net-mvc-4', 0.08).
related('asp.net-mvc', 'asp.net-mvc-5', 0.05).
related('c#', 'asp.net-mvc-5', 0.05).
related('asp.net', 'asp.net-membership', 0.05).
related('jquery', 'asp.net-ajax', 0.00).
related('asp.net', 'asp.net-ajax', 0.03).
related('ajax', 'asp.net-ajax', 0.02).
related('asp.net', 'asp.net-authentication', 0.01).
related('asp.net', 'asp.net-mvc-4', 0.04).
related('mysql', 'c', 0.06).
related('c++', 'c', 0.41).
related('pointers', 'c', 0.19).
related('linux', 'c', 0.12).
related('arrays', 'c', 0.10).
related('struct', 'c', 0.04).
related('winapi', 'c', 0.07).
related('visual-studio', 'c', 0.12).
related('multithreading', 'c', 0.04).
related('file', 'c', 0.08).
related('assembly', 'c', 0.09).
related('memory', 'c', 0.11).
related('gcc', 'c', 0.06).
related('pthreads', 'c', 0.04).
related('c++', 'c#', 0.10).
related('wpf', 'c#', 0.21).
related('asp.net', 'c#', 0.62).
related('asp.net-mvc', 'c#', 0.53).
related('linq', 'c#', 0.12).
related('winforms', 'c#', 0.20).
related('sql', 'c#', 0.11).
related('xml', 'c#', 0.10).
related('visual-studio', 'c#', 0.29).
related('sql-server', 'c#', 0.25).
related('java', 'c#', 0.11).
related('generics', 'c#', 0.13).
related('events', 'c#', 0.01).
related('wcf', 'c#', 0.19).
related('nhibernate', 'c#', 0.16).
related('silverlight', 'c#', 0.17).
related('oop', 'c#', 0.11).
related('reflection', 'c#', 0.12).
related('performance', 'c#', 0.11).
related('mysql', 'c#', 0.12).
related('xaml', 'c#', 0.16).
related('string', 'c#', 0.10).
related('wcf', 'c#-4.0', 0.04).
related('.net', 'c#-4.0', 0.05).
related('c#', 'c#-4.0', 0.12).
related('silverlight', 'c#-4.0', 0.08).
related('linq', 'c#-4.0', 0.03).
related('visual-studio-2010', 'c#-4.0', 0.06).
related('c#', 'casting', 0.04).
related('linq', 'casting', 0.01).
related('java', 'casting', 0.03).
related('c++', 'casting', 0.07).
related('mysql', 'codeigniter', 0.08).
related('php', 'codeigniter', 0.19).
related('database', 'codeigniter', 0.08).
related('activerecord', 'codeigniter', 0.02).
related('html', 'codeigniter', 0.02).
related('javascript', 'codeigniter', 0.02).
related('ajax', 'codeigniter', 0.03).
related('jquery', 'codeigniter', 0.05).
related('asp.net', 'controls', 0.03).
related('visual-studio', 'c++', 0.27).
related('visual-c++', 'c++', 0.21).
related('winapi', 'c++', 0.17).
related('stl', 'c++', 0.30).
related('templates', 'c++', 0.21).
related('pointers', 'c++', 0.13).
related('windows', 'c++', 0.08).
related('boost', 'c++', 0.22).
related('qt', 'c++', 0.22).
related('mfc', 'c++', 0.15).
related('c', 'c++', 0.41).
related('opengl', 'c++', 0.14).
related('c#', 'c++', 0.10).
related('multithreading', 'c++', 0.14).
related('cmake', 'c++', 0.05).
related('directx', 'c++', 0.12).
related('eclipse', 'c++', 0.13).
related('ansi-c++', 'c++', 0.17).
related('mysql', 'database', 0.20).
related('sql-server', 'database', 0.22).
related('sql', 'database', 0.19).
related('schema', 'database', 0.06).
related('nosql', 'database', 0.11).
related('stored-procedures', 'database', 0.09).
related('oracle', 'database', 0.18).
related('normalization', 'database', 0.08).
related('tsql', 'database', 0.11).
related('ruby-on-rails', 'database', 0.13).
related('indexing', 'database', 0.12).
related('performance', 'database', 0.10).
related('sql-server-2005', 'database', 0.11).
related('lucene', 'database', 0.16).
related('linq-to-sql', 'database', 0.13).
related('sqlite', 'database', 0.13).
related('backup', 'database', 0.13).
related('postgresql', 'database', 0.14).
related('excel', 'database', 0.08).
related('transactions', 'database', 0.07).
related('reporting-services', 'database', 0.15).
related('entity-framework', 'database', 0.14).
related('php', 'database', 0.17).
related('ssis', 'database', 0.15).
related('hibernate', 'database', 0.14).
related('asp.net-mvc', 'database', 0.10).
related('oracle', 'datetime', 0.03).
related('java', 'datetime', 0.03).
related('jquery', 'datetime', 0.03).
related('c#', 'datetime', 0.04).
related('asp.net', 'datetime', 0.09).
related('mysql', 'datetime', 0.07).
related('sql', 'datetime', 0.07).
related('php', 'datetime', 0.08).
related('wpf', 'dependency-properties', 0.10).
related('winforms', 'devexpress', 0.02).
related('wpf', 'devexpress', 0.02).
related('asp.net-mvc', 'devexpress', 0.02).
related('objective-c', 'dynamic', 0.04).
related('linq', 'entity-framework', 0.11).
related('entity-framework-4', 'entity-framework', 0.08).
related('.net', 'entity-framework', 0.04).
related('wpf', 'entity-framework', 0.05).
related('linq-to-entities', 'entity-framework', 0.11).
related('asp.net', 'entity-framework', 0.06).
related('unit-testing', 'entity-framework', 0.09).
related('sql', 'entity-framework', 0.08).
related('linq-to-sql', 'entity-framework', 0.04).
related('sql-server', 'entity-framework', 0.11).
related('winforms', 'entity-framework', 0.05).
related('asp.net-mvc-3', 'entity-framework', 0.10).
related('silverlight', 'entity-framework', 0.04).
related('validation', 'entity-framework', 0.08).
related('c#', 'exception', 0.06).
related('java', 'exception', 0.05).
related('wpf', 'expression-blend', 0.07).
related('wpf', 'fscommand', 0.01).
related('c#', 'garbage-collection', 0.03).
related('java', 'garbage-collection', 0.07).
related('python', 'garbage-collection', 0.02).
related('lisp', 'garbage-collection', 0.07).
related('c', 'gcc', 0.06).
related('c++', 'gcc', 0.07).
related('javascript', 'google-chrome', 0.02).
related('jquery', 'google-chrome', 0.04).
related('wpf', 'graphics', 0.09).
related('gdi+', 'graphics', 0.17).
related('java', 'graphics', 0.07).
related('opengl', 'graphics', 0.13).
related('silverlight', 'graphics', 0.14).
related('wcf', 'hosting', 0.05).
related('asp.net', 'http', 0.04).
related('asp.net', 'iis', 0.09).
related('.net', 'iis', 0.02).
related('web-config', 'iis', 0.03).
related('deployment', 'iis', 0.06).
related('asp.net-mvc', 'iis', 0.02).
related('wcf', 'iis', 0.05).
related('web-services', 'iis', 0.06).
related('apache', 'iis', 0.03).
related('mysql', 'indexing', 0.07).
related('sql', 'indexing', 0.06).
related('database', 'indexing', 0.12).
related('sql-server', 'indexing', 0.16).
related('mysql', 'innodb', 0.07).
related('c#', 'interface', 0.02).
related('java', 'interface', 0.06).
related('java', 'intellij-idea', 0.06).
related('html', 'internet-explorer', 0.05).
related('javascript', 'internet-explorer', 0.07).
related('css', 'internet-explorer', 0.05).
related('jquery', 'internet-explorer', 0.07).
related('ajax', 'internet-explorer', 0.05).
related('php', 'internet-explorer', 0.03).
related('c#', 'internet-explorer', 0.02).
related('css3', 'internet-explorer', 0.03).
related('php', 'javascript', 0.19).
related('jquery', 'javascript', 0.60).
related('css', 'javascript', 0.23).
related('html', 'javascript', 0.28).
related('ajax', 'javascript', 0.33).
related('dom', 'javascript', 0.35).
related('json', 'javascript', 0.28).
related('regex', 'javascript', 0.18).
related('jquery-ui', 'javascript', 0.09).
related('oop', 'javascript', 0.16).
related('validation', 'javascript', 0.07).
related('facebook', 'javascript', 0.13).
related('forms', 'javascript', 0.06).
related('asp.net', 'javascript', 0.11).
related('html5', 'javascript', 0.19).
related('javascript', 'jquery', 0.60).
related('css', 'jquery', 0.26).
related('html', 'jquery', 0.21).
related('ajax', 'jquery', 0.22).
related('jquery-ui', 'jquery', 0.34).
related('validation', 'jquery', 0.14).
related('json', 'jquery', 0.24).
related('forms', 'jquery', 0.20).
related('css3', 'jquery', 0.14).
related('asp.net', 'jquery', 0.40).
related('selector', 'jquery', 0.14).
related('html5', 'jquery', 0.19).
related('events', 'jquery', 0.05).
related('internet-explorer', 'jquery', 0.07).
related('php', 'jquery', 0.24).
related('asp.net-mvc', 'jquery', 0.29).
related('html', 'jsf', 0.03).
related('servlets', 'jsf', 0.09).
related('tomcat', 'jsf', 0.08).
related('primefaces', 'jsf', 0.09).
related('regex', 'multithreading', 0.01).
related('java', 'multithreading', 0.29).
related('c#', 'multithreading', 0.10).
related('python', 'multithreading', 0.07).
related('c++', 'multithreading', 0.14).
related('c', 'multithreading', 0.04).
related('wcf', 'nhibernate', 0.10).

related('wpf', 'nhibernate', 0.04).
related('c#', 'nhibernate', 0.16).
related('asp.net', 'nhibernate', 0.09).
related('linq', 'nhibernate', 0.07).
related('fluent-nhibernate', 'nhibernate', 0.08).
related('nhibernate', 'orm', 0.08).
related('hibernate', 'orm', 0.07).
related('entity-framework', 'orm', 0.05).
related('ruby-on-rails', 'orm', 0.01).
related('mysql', 'performance', 0.10).
related('sql', 'performance', 0.08).
related('asp.net', 'performance', 0.06).
related('wpf', 'performance', 0.08).
related('sql-server', 'performance', 0.13).
related('sql-server-2005', 'performance', 0.13).
related('database', 'performance', 0.10).
related('linq', 'performance', 0.07).
related('c#', 'performance', 0.11).
related('java', 'performance', 0.11).
related('oracle', 'performance', 0.10).
related('c++', 'performance', 0.12).
related('visual-studio', 'performance', 0.09).
related('python', 'performance', 0.10).
related('sql-server', 'reporting-services', 0.11).
related('database', 'reporting-services', 0.15).
related('ssis', 'reporting-services', 0.16).
related('vb.net', 'reporting-services', 0.06).
related('vba', 'reporting-services', 0.06).
related('asp.net', 'rest', 0.03).
related('wcf', 'rest', 0.07).
related('iphone', 'rest', 0.05).
related('asp.net-mvc', 'routing', 0.07).
related('asp.net', 'routing', 0.04).
related('iis', 'routing', 0.02).
related('linq', 'select', 0.02).
related('wpf', 'silverlight', 0.07).
related('wcf', 'silverlight', 0.08).
related('databinding', 'silverlight', 0.11).
related('.net', 'silverlight', 0.03).
related('vb.net', 'silverlight', 0.03).
related('c#', 'silverlight', 0.17).
related('wcf-ria-services', 'silverlight', 0.07).
related('xaml', 'silverlight', 0.12).
related('mvvm', 'silverlight', 0.07).
related('c#', 'singleton', 0.05).
related('java', 'singleton', 0.05).
related('objective-c', 'singleton', 0.06).
related('python', 'singleton', 0.02).
related('mysql', 'sql', 0.28).
related('sql-server', 'sql', 0.50).
related('database', 'sql', 0.19).
related('oracle', 'sql', 0.17).
related('linq', 'sql', 0.03).
related('tsql', 'sql', 0.25).
related('postgresql', 'sql', 0.14).
related('.net', 'sql', 0.03).
related('c#', 'sql', 0.11).
related('join', 'sql', 0.17).
related('jsp', 'struts', 0.16).
related('spring', 'struts', 0.21).
related('hibernate', 'struts', 0.17).
related('java', 'struts', 0.20).
related('servlets', 'struts', 0.19).
related('tomcat', 'struts', 0.16).
related('wpf', 'styles', 0.15).
related('java', 'swing', 0.24).
related('layout-manager', 'swing', 0.06).
related('multithreading', 'swing', 0.05).
related('wpf', 'templates', 0.09).
related('ant', 'tomcat', 0.08).
related('mysql', 'tomcat', 0.07).
related('eclipse', 'tomcat', 0.10).
related('jenkins', 'tomcat', 0.02).
related('struts', 'tomcat', 0.16).
related('servlets', 'tomcat', 0.21).
related('jsp', 'tomcat', 0.15).
related('java', 'tomcat', 0.13).
related('wcf', 'unit-testing', 0.10).
related('entity-framework', 'unit-testing', 0.09).
related('nhibernate', 'unit-testing', 0.09).
related('asp.net-mvc', 'unit-testing', 0.11).
related('asp.net-mvc-2', 'unit-testing', 0.00).
related('javascript', 'validation', 0.07).
related('jquery', 'validation', 0.14).
related('html', 'validation', 0.07).
related('asp.net', 'validation', 0.10).
related('entity-framework', 'validation', 0.08).
related('javascript', 'visual-studio', 0.01).
related('.net', 'visual-studio', 0.10).
related('c++', 'visual-studio', 0.27).
related('wpf', 'visual-studio', 0.09).
related('svn', 'visual-studio', 0.14).
related('c#', 'visual-studio', 0.29).
related('resharper', 'visual-studio', 0.14).
related('visual-studio-2010', 'visual-studio', 0.20).
related('visual-c++', 'visual-studio', 0.21).
related('tfs', 'visual-studio', 0.09).
related('visual-studio-addins', 'visual-studio', 0.09).
related('.net', 'visual-studio-2008', 0.02).
related('wpf', 'visual-studio-2008', 0.03).
related('c#', 'visual-studio-2008', 0.17).
related('asp.net', 'visual-studio-2008', 0.06).
related('wpf', 'visual-studio-2010', 0.06).
related('wcf', 'visual-studio-2010', 0.05).
related('.net', 'visual-studio-2010', 0.05).
related('wpf', 'visual-studio-2010-express', 0.02).
related('.net', 'visual-studio-2012', 0.03).
related('wpf', 'windows-forms', 0.03).
related('c#', 'windows-forms-designer', 0.01).
related('linq', 'winforms', 0.03).
related('.net', 'winforms', 0.07).
related('c#', 'winforms', 0.20).
related('wpf', 'winforms', 0.08).
related('wpf', 'winforms-interop', 0.11).
related('mysql', 'wpf', 0.07).
related('wcf', 'wpf', 0.05).
related('c#', 'wpf', 0.21).
related('.net', 'wpf', 0.12).
related('xaml', 'wpf', 0.12).
related('mvvm', 'wpf', 0.17).
related('databinding', 'wpf', 0.17).
related('datagrid', 'wpf', 0.08).
related('listview', 'wpf', 0.09).
related('listbox', 'wpf', 0.09).
related('templates', 'wpf', 0.09).
related('resources', 'wpf', 0.09).
related('user-controls', 'wpf', 0.09).
related('visual-studio', 'wpf', 0.09).
related('nhibernate', 'wpf', 0.04).
related('entity-framework', 'wpf', 0.05).
related('events', 'wpf', 0.05).
related('animation', 'wpf', 0.06).
related('devexpress', 'wpf', 0.02).
related('styles', 'wpf', 0.15).
related('layout', 'wpf', 0.10).
related('validation', 'wpf', 0.09).
related('silverlight', 'wpf', 0.07).
related('controls', 'wpf', 0.09).
related('resources', 'xaml', 0.04).
related('databinding', 'xaml', 0.08).
related('wpf', 'xaml', 0.12).
related('c#', 'xaml', 0.16).
related('silverlight', 'xaml', 0.12).
related('windows-phone-7', 'xaml', 0.08).
related('haskell', 'xml', 0.04).
related('c#', 'xml', 0.10).
related('java', 'xml', 0.14).
related('php', 'xml', 0.10).
related('javascript', 'xml', 0.03).
related('xslt', 'xml', 0.11).
related('linq-to-xml', 'xml', 0.10).
related('dom', 'xml', 0.08).
related('apache', 'xml', 0.03).
related('asp.net', 'xml', 0.05).
related('android', 'xml', 0.24).
related('opengl', 'xna', 0.02).
related('c#', 'xna', 0.08).
related('visual-studio', 'xna', 0.04).
related('asp.net', 'xml', 0.05).
related('mysql', 'android', 0.05).
related('c#', 'android', 0.08).
related('sqlite', 'android', 0.25).
related('eclipse', 'android', 0.43).
related('xml', 'android', 0.24).
related('json', 'android', 0.25).
related('webview', 'android', 0.16).
related('java', 'android', 0.58).
related('listview', 'android', 0.22).
related('layout', 'android', 0.23).
related('iphone', 'android', 0.11).
related('windows-phone-7', 'android', 0.08).
related('mysql', 'java', 0.18).
related('hibernate', 'java', 0.23).
related('swing', 'java', 0.24).
related('eclipse', 'java', 0.39).
related('spring', 'java', 0.28).
related('jsp', 'java', 0.22).
related('servlets', 'java', 0.19).
related('tomcat', 'java', 0.13).
related('xml', 'java', 0.14).
related('generics', 'java', 0.19).
related('android', 'java', 0.58).
related('multithreading', 'java', 0.29).
related('reflection', 'java', 0.07).
related('performance', 'java', 0.11).
related('jpa', 'java', 0.08).
related('struts', 'java', 0.20).
related('spring-mvc', 'java', 0.06).
related('iphone', 'objective-c', 0.56).
related('cocoa-touch', 'objective-c', 0.45).
related('ios', 'objective-c', 0.53).
related('ipad', 'objective-c', 0.22).
related('uitableview', 'objective-c', 0.24).
related('xcode', 'objective-c', 0.35).
related('macos', 'objective-c', 0.17).
related('uiview', 'objective-c', 0.22).
related('blocks', 'objective-c', 0.17).
related('json', 'objective-c', 0.16).
related('memory-management', 'objective-c', 0.19).
related('core-data', 'objective-c', 0.19).
related('nsstring', 'objective-c', 0.10).
related('iphone', 'ios', 0.53).
related('ipad', 'ios', 0.32).
related('uitableview', 'ios', 0.31).
related('xcode', 'ios', 0.25).
related('macos', 'ios', 0.09).
related('android', 'iphone', 0.11).
related('cocoa-touch', 'iphone', 0.46).
related('objective-c', 'iphone', 0.56).
related('ios', 'iphone', 0.53).
related('ipad', 'iphone', 0.26).
related('xcode', 'iphone', 0.36).
related('uitableview', 'iphone', 0.32).
related('navigation', 'iphone', 0.17).
related('json', 'iphone', 0.14).
related('uiview', 'iphone', 0.22).
related('core-data', 'iphone', 0.16).
related('macos', 'iphone', 0.10).
related('arrays', 'php', 0.09).
related('mysql', 'php', 0.27).
related('javascript', 'php', 0.19).
related('apache', 'php', 0.12).
related('oop', 'php', 0.12).
related('codeigniter', 'php', 0.19).
related('email', 'php', 0.19).
related('jquery', 'php', 0.24).
related('html', 'php', 0.20).
related('zend-framework', 'php', 0.13).
related('ajax', 'php', 0.25).
related('validation', 'php', 0.07).
related('curl', 'php', 0.16).
related('xml', 'php', 0.10).
related('ports', 'python', 1.00).
related('mysql', 'python', 0.10).
related('lambda', 'python', 0.09).
related('multithreading', 'python', 0.07).
related('performance', 'python', 0.10).
related('regex', 'python', 0.12).
related('memory', 'python', 0.08).
related('dictionary', 'python', 0.13).
related('c#', 'python', 0.08).
related('c++', 'python', 0.08).
related('windows', 'python', 0.07).
related('django', 'python', 0.22).
related('cocoa-touch', 'sdk', 0.09).
related('iphone', 'sdk', 0.09).
related('ios', 'sdk', 0.07).
related('python', 'singleton', 0.02).
related('vb.net', 'sql', 0.05).
related('sql-server-2005', 'sql', 0.15).
related('c#', 'sql-server', 0.25).
related('sql', 'sql-server', 0.50).
related('asp.net', 'sql-server', 0.27).
related('tsql', 'sql-server', 0.35).
related('database', 'sql-server', 0.23).
related('sql-server-2005', 'sql-server', 0.32).
related('visual-studio', 'sql-server', 0.19).
related('wpf', 'sql-server', 0.05).
related('.net', 'sql-server', 0.12).
related('reporting-services', 'sql-server', 0.11).
related('sql-server-2008', 'sql-server', 0.54).
related('performance', 'sql-server', 0.13).
related('backup', 'sql-server', 0.11).
related('database-design', 'sql-server', 0.13).
related('schema', 'sql-server', 0.08).
related('indexing', 'sql-server', 0.16).
related('stored-procedures', 'sql-server', 0.12).
related('ssis', 'sql-server', 0.14).
related('mysql', 'sql-server', 0.16).
related('oracle', 'sql-server', 0.14).
related('visual-studio', 'svn', 0.14).
related('eclipse', 'svn', 0.24).
related('c#', 'svn', 0.06).
related('visual-studio', 'tfs', 0.09).
related('asp.net', 'tfs', 0.01).
related('c#', 'tfs', 0.02).
related('jquery', 'uiwebview', 0.05).
related('asp.net-mvc-3', 'validation', 0.05).
related('asp.net-mvc', 'validation', 0.10).
related('asp.net-mvc-2', 'validation', 0.03).
related('wpf', 'validation', 0.09).
related('silverlight', 'validation', 0.04).
related('asp.net', 'validations', 0.01).
related('asp.net-mvc', 'viewdata', 0.01).
related('asp.net-mvc', 'viewmodel', 0.06).
related('wpf', 'viewmodel', 0.11).
related('asp.net-mvc', 'views', 0.10).
related('php', 'zend-framework', 0.13).



% canonically-viewed symmetric edge
edge(Xc, Yc, W) :-
    related(X, Y, W),
    canonical(X, Xc),
    canonical(Y, Yc).

edge(Xc, Yc, W) :-
    related(Y, X, W),
    canonical(X, Xc),
    canonical(Y, Yc).


% Aliases

alias('c#','csharp').
alias('microsoft_sql_server','mssql').
alias('sql','mssql').


canonical(T, C) :-
    ( alias(T, A) -> C = A ; C = T ).


% Resume / Job Facts 

:- dynamic resume_term/3.
:- dynamic job_term/4.


% Temporal Utilities. We convert to YYYYMM format for ease of calculation, we also calculate time ranges here

ym_to_ym(YM, Y, M) :- Y is YM // 100, M is YM mod 100.

months_between(YM1, YM2, Months) :-
    ym_to_ym(YM1, Y1, M1),
    ym_to_ym(YM2, Y2, M2),
    Months is (Y2 - Y1) * 12 + (M2 - M1) + 1.

months_intersection(Start1, End1, Start2, End2, 0) :-
    ( End1 < Start2 ; End2 < Start1 ), !.
months_intersection(Start1, End1, Start2, End2, Months) :-
    MaxStart is max(Start1, Start2),
    MinEnd is min(End1, End2),
    months_between(MaxStart, MinEnd, Months).

% If the job has no timeframe requirement, ignore time completely
temporal_factor(_RStart, _REnd, none, none, 1.0) :- !.


temporal_factor(RStart, REnd, JStart, JEnd, Factor) :-
    JStart \= none, JEnd \= none, !,
    months_intersection(RStart, REnd, JStart, JEnd, Overlap),
    (Overlap = 0 -> Factor = 0.0 ;
     months_between(JStart, JEnd, JDur),
     Factor is Overlap / JDur).

temporal_factor(RStart, REnd, JStart, none, Factor) :-
    JStart \= none,
    months_intersection(RStart, REnd, JStart, 999912, Overlap),
    (Overlap = 0 -> Factor = 0.0 ;
     Factor0 is Overlap / 36,
     Factor is min(1.0, Factor0)).


% We assign weights to edges in the graph based on relatedness

graph_similarity(A, B, S) :-
    canonical(A, Ac),
    canonical(B, Bc),
    ( Ac == Bc -> S = 1.0
    ; edge(Ac, Bc, W) -> S = W
    ; S = 0.0 ).


% We calculate pair scores for job/resume term pairs and then determine best matches

pair_score(JTerm, RTerm, Importance, PairScore, details{sim:Sim, temp:TF}) :-
    graph_similarity(JTerm, RTerm, Sim),
    resume_term(RTerm, RStart, REnd),
    job_term(JTerm, Importance, JStart, JEnd),
    temporal_factor(RStart, REnd, JStart, JEnd, TF),
    Raw is Sim * TF,
    PairScore is Importance * Raw.

best_match_for_job(JTerm, BestRTerm, BestScore, Details) :-
    findall( pair(RTerm,Score,Det),
             ( resume_term(RTerm,_,_),
               pair_score(JTerm,RTerm,_,Score,Det) ),
             Pairs),
    Pairs \= [], !,
    sort(2, @>=, Pairs, Sorted),
    Sorted = [ pair(BestRTerm, BestScore, Details) | _ ].
best_match_for_job(_, none, 0.0, _{}).


% We compute the final score and breakdown based on weighted averages

final_score(Score, Breakdown) :-
    findall(Importance, job_term(_, Importance, _, _), Imps),
    sum_list(Imps, Den),
    Den > 0,
    findall(entry(JTerm,Importance,BestRTerm,PairScore,Sim,TF),
      ( job_term(JTerm, Importance, JStart, JEnd),
        best_match_for_job(JTerm, BestRTerm, PairScore,
                           details{sim:Sim,temp:TF})
      ),
      Breakdown),
    findall(PS, member(entry(_,_,_,PS,_,_),Breakdown), PSs),
    sum_list(PSs, Num),
    Score is Num / Den.
