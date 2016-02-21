USE employees;
-- 2.4.a.i
-- calculate average and std employee age
SELECT ROUND(AVG(TIMESTAMPDIFF(DAY,birth_date,hire_date)/365),3) AS AvgAge
,     ROUND(STDDEV_SAMP(TIMESTAMPDIFF(DAY,birth_date,hire_date)/365),3) AS StdAge
FROM employees;

-- calculate median employee age
SELECT count(*)/2 from employees;
SELECT TIMESTAMPDIFF(DAY,birth_date,hire_date)/365 AS MedianAge
FROM employees
ORDER BY MedianAge
LIMIT 2 OFFSET 150012;

-- 2.4.a.ii
-- avg and std salary for all employees
  SELECT ROUND(AVG(salary),3)     AS AvgSalary
,       ROUND(STDDEV_SAMP(salary),3)  AS StDevSalary
  FROM finalsalary;
  
  -- median salary for all employees
  SELECT count(*)/2 FROM finalsalary;
  SELECT salary AS MedSalary
  FROM finalsalary 
  ORDER BY salary
  LIMIT 3 OFFSET 150011; 
  
 -- 2.4.a.iii salary by gender
  -- avg and std by gender
  /* 
  (AvgSalary, StDevSalary, gender) VALUES
  (69939.619, 17678.998, 'M'), 
  (69912.524, 17579.713, 'F')*/
  SELECT ROUND(AVG(s.salary),3)     AS AvgSalary
,       ROUND(STDDEV(s.salary),3)  AS StDevSalary
,       e.gender
  FROM finalsalary                 AS s
  LEFT JOIN employees           AS e
  ON   s.emp_no = e.emp_no
  GROUP BY e.gender;

  -- median salary for males
  -- $58,972.00
  SELECT gender, count(*)/2 FROM employees GROUP BY gender;
  SELECT s.salary
  FROM finalsalary                 AS s
  INNER JOIN employees           AS e
  ON   s.emp_no = e.emp_no
  AND e.gender = 'M'
  ORDER BY s.salary
  LIMIT 1 OFFSET 89986;
  
  -- median salary for females
  -- $54,114.00
  SELECT s.salary
  FROM finalsalary                 AS s
  INNER JOIN employees           AS e
  ON   s.emp_no = e.emp_no
  AND e.gender = 'F'
  ORDER BY s.salary
  LIMIT 1 OFFSET 60025;
 
 
 -- average and stdev of salary by department
 /*
  (dept_name, average, std) VALUES
  ('Customer Service', 64963.582, 16079.86), 
  ('Development', 65600.754, 14591.254), 
  ('Finance', 76569.223, 17384.978), 
  ('Human Resources', 61724.993, 13237.341), 
  ('Marketing', 77973.995, 17713.822), 
  ('Production', 65745.657, 14629.859), 
  ('Quality Management', 63324.398, 13730.142), 
  ('Research', 65806.554, 14735.104), 
  ('Sales', 86794.909, 18092.435)
*/
 SELECT d.dept_name
 , ROUND(AVG(s.salary),3) AS average
 , ROUND(STD(s.salary),3) AS std
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 LEFT JOIN departments AS d
  ON d.dept_no = de.dept_no
 GROUP BY d.dept_name
 ORDER BY d.dept_no;
 
 SELECT count(DISTINCT(emp_no)) FROM final_dept;
 
 -- median salary by dept
 /*
 (dept_name, median) VALUES
('Customer Service', 10906.5000), 
('Development', 38479.0000), 
('Finance', 7789.5000), 
('Human Resources', 8035.5000), 
('Marketing', 9213.0000), 
('Production', 33337.5000), 
('Quality Management', 9147.5000), 
('Research', 9642.5000), 
('Sales', 23461.0000)
 */
 SELECT d.dept_name
 , count(*)/2 AS median
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 INNER JOIN departments AS d
  ON d.dept_no = de.dept_no
 GROUP BY d.dept_name
 ORDER BY d.dept_no;
 
 -- Customer service
 SELECT count(*)/2 FROM final_dept WHERE dept_no = 'd009';
 SELECT d.dept_name
 , s.salary
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 INNER JOIN departments AS d
  ON d.dept_no = de.dept_no
 WHERE d.dept_name = 'Customer Service'
 ORDER BY s.salary ASC
 LIMIT 3 OFFSET 10905;
 SELECT (63035+63038+63038)/3;

-- Development
SELECT d.dept_name
 , s.salary
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 INNER JOIN departments AS d
  ON d.dept_no = de.dept_no
 WHERE d.dept_name = 'Development'
 ORDER BY s.salary
 LIMIT 3 OFFSET 38478;
 
 -- Finance
SELECT d.dept_name
 , s.salary
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 INNER JOIN departments AS d
  ON d.dept_no = de.dept_no
 WHERE d.dept_name = 'Finance'
 ORDER BY s.salary
 LIMIT 1 OFFSET 7789;
 
 -- Human Resources
SELECT d.dept_name
 , s.salary
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 INNER JOIN departments AS d
  ON d.dept_no = de.dept_no
 WHERE d.dept_name = 'Human Resources'
 ORDER BY s.salary ASC
 LIMIT 1 OFFSET 8035;
 
 -- Marketing
SELECT d.dept_name
 , s.salary
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 INNER JOIN departments AS d
  ON d.dept_no = de.dept_no
 WHERE d.dept_name = 'Marketing'
 ORDER BY s.salary
 LIMIT 3 OFFSET 9212;

 
 -- Production
SELECT d.dept_name
 , s.salary
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 INNER JOIN departments AS d
  ON d.dept_no = de.dept_no
 WHERE d.dept_name = 'Production'
 ORDER BY s.salary
 LIMIT 2 OFFSET 33337;
 SELECT (64594+64596)/2;
 
 -- Quality Management
SELECT d.dept_name
 , s.salary
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 INNER JOIN departments AS d
  ON d.dept_no = de.dept_no
 WHERE d.dept_name = 'Quality Management'
 ORDER BY s.salary
 LIMIT 2 OFFSET 9146;
 
 -- Research
SELECT d.dept_name
 , s.salary
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 INNER JOIN departments AS d
  ON d.dept_no = de.dept_no
 WHERE d.dept_name = 'Research'
 ORDER BY s.salary
 LIMIT 1 OFFSET 9642;
 
  -- Sales
SELECT d.dept_name
 , s.salary
 FROM finalsalary AS s
 INNER JOIN final_dept AS de
  ON de.emp_no = s.emp_no
 INNER JOIN departments AS d
  ON d.dept_no = de.dept_no
 WHERE d.dept_name = 'Sales'
 ORDER BY s.salary
 LIMIT 2 OFFSET 23461;
-------------------------------------------

-- PART B
-- average for managers
SELECT d.dept_name
 , ROUND(AVG(s.salary),3) AS average
 , count(*)
 FROM finalsalary AS s
 INNER JOIN dept_manager AS dm
  ON dm.emp_no = s.emp_no
 LEFT JOIN departments AS d
  ON d.dept_no = dm.dept_no
 GROUP BY d.dept_name
 ORDER BY d.dept_no;
 
 -- median will be average for all depts that have 2 managers
 SELECT d.dept_name
 , MAX(dm.emp_no)
 FROM finalsalary AS s
 INNER JOIN dept_manager AS dm
  ON dm.emp_no = s.emp_no
 LEFT JOIN departments AS d
  ON d.dept_no = dm.dept_no
 GROUP BY d.dept_no
 ORDER BY d.dept_no;
 
 -- find salary for current manager
 SELECT d.dept_name, s.salary 
 FROM finalsalary AS s 
 INNER JOIN dept_manager AS dm
  ON dm.emp_no = s.emp_no
 LEFT JOIN departments as d
  ON d.dept_no = dm.dept_no
 WHERE s.emp_no = 110039
 OR s.emp_no = 110114
 OR s.emp_no = 110228
 OR s.emp_no = 110420
 OR s.emp_no = 110567
 OR s.emp_no = 110854
 OR s.emp_no = 111133
 OR s.emp_no = 111534
 OR s.emp_no = 111939
 ;

