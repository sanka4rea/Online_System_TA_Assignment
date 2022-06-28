# Online R-shiny system of TA Assignment
<font size=4>

Key codes for the very simple Online System for TA Assignment based on R shiny. 
Used for backup and explaination to lab members.</font>


## 1. Prerequisites 
### (Docker/rstudio/ssh start)
1. 67上`docker ps –a`
   
2. 开启container(BMS_TA), `docker restart xxxxxxxx`
   
3. 进入container, `docker exec -it xxxxxxxx /bin/bash`
   
4. 开启rstudioserver，ssh, `cd /etc/init.d` + `./rstudio-server start` + `./ssh start`. (shiny默认开启)
   
5. 此时拥有此容器的root权限，可以新增用户（现在Rstudio：user：`qilin`, pw:`qilin`）
   
6. 登录rstudio web，端口: 67:5001 ，进行coding/testing或者本地testing，用户名和密码都是`qilin`, 在`/home/qilin/shiny`中或者本地进行调试，确认后将最终版本更新到`/srv/shiny-server/BMS_TA`中
   
7. shiny web：http://xxxxxxxx (正式对外地址，对应`/srv/shiny-server/BMS_TA`,testing时确保**不要真的发出去邮件**,去除excel中的邮箱信息)

## 2. Configuration

1. folder: /email/.Rmd, 对应修改email template （根据GO每年发的email模板进行对应修改）
   
2. 删除data中的旧数据和student passwd.rdata
   
3. 检查Course Leader Info文件中的邮箱是否有末尾/开头空格，如有则去除，
   确保和course requirement中的邮箱相匹配。
   检查列名，检查起始行数，以更新init.R, server.R, functions.R （检查code中读取的行数是否完全涵盖所有行）

4. 检查TA info中的邮箱是否有末尾/开头空格，否则会导致邮件发送中断（如不检查，也可以从中断处修改后继续发送，**谨慎操作防止重复发送邮件**）
   检查列名，检查起始行数，以更新init.R, server.R, functions.R（检查code中读取的行数是否完全涵盖所有行）

5. Course requirement: 注意检查perference的分隔符。<del>将TA时间统一为单独列</del>

## 3. Workflow (together with the instruction for `Instruction for TA system2021.pdf`)

0. login as student for testing, login as administrator for management/email sending
   
1. 检查course infomation是否正确
   
2. 更新最终版本后，确保赋予shiny文件夹写入权限/777，才能写入（generate student passwd .rdata）

3. Manage student submission: Generate student's accounts and send emails, 给学生发送包含用户名，密码的邮件

4. 当deadline后，系统中Manage student submission:Update student submission，再Confirm all student submission，生成了.rdata

5. Export Submission:导出xlsx格式的学生们的填报信息，发送给GO,等优化
   
<del>6. （现在已经不需要的环节）自动分配TA。首先点击Manage student submission:Confirm all student submission(导入rdata)。Optimization for assignment-->Confirm and save the assignment result-->Export the assignment result，发送给GO修改-->Send preliminary results emails to CLs.</del>

7. GO返回优化后的表格，需要进行格式修改才能读入系统。（从每个学生的对应duty版本，变更为每个course的对应学生版本）：`transfer_result_to_server.R`
   注意检查生成的结果，是否有空值。可能优化后的表格里面存在 course1,course2,course3。

8. 注意GO有无添加新的TA，即不在原Ta_list.xlsx里面，则需要更新Ta_list.xlsx，并且重新在系统中生成学生帐号密码的rdata（注意手动生成添加在已有的rdata后面即可，不要发送邮件,否则导致所有学生重新收到用户名密码邮件）
   
9.  导入GO优化后的表格，`Upload new TA assignment file-->Confirm new TA assignment`,检查格式和信息是否完整

10. 进行本地测试（使用自己的邮箱进行接收），检查发送给老师的邮件格式是否正确，没问题后online发送给course leaders（**慎重!!!**：**Send preliminary emails to CLs**,一定先检查确保无误再正式发送）

11.  等候GO通知，是否有进一步的修改。如果有更改，对应的TA表格也要更改，用`transfer_result_to_server.R`重新生成course对应学生的版本，`Upload new TA assignment file-->Confirm new TA assignment`,再发送给学生`Send final results to Students` （**慎重!!!**：一定先检查确保无误再正式发送）

12. Congradulations! Finished.