The UK Gender Pay Gap
================
Simon Hulme
2022-09-16

# Overview

From 2017, any employer who has a headcount of 250 or more on their
‘snapshot date’ must comply with regulations on gender pay gap
reporting. This requires employers to annually report and publish
specific figures about their gender pay gap.

## **What is the gender pay gap?**

The gender pay gap is the difference between the average (mean or
median) earnings of men and women across a workforce. This is expressed
as a percentage of men’s earnings. For example, ‘women earn 15% less
than men per hour’. The gender pay gap can be calculated across a whole
workforce, but also for subgroups. For example, based on age or work
patterns like part-time work. This helps to understand if certain
subgroups are affected more than others.

### **The gender pay gap is not the same as equal pay**

While the gender pay gap and equal pay both deal with pay disparity at
work, they are not the same issue.

Equal pay means that men and women performing equal work, or work of
equal value, must receive equal pay. This is the law and employers must
observe it. This applies not only to salary, but to all contractual
terms and conditions of employment, such as holiday entitlement,
bonuses, pay and reward schemes, pension payments and other benefits.
Unequal pay has been unlawful for decades and is now covered under the
Equality Act 2010.

The gender pay gap is a measure of the difference between men and
women’s average earnings across an organisation or the labour market as
a whole over a period of time, regardless of role or seniority. Even if
an employer has an effective equal pay policy, it could still have a
gender pay gap if, for example, the majority of women are employed in
lower-paid jobs.

## **Regulations**

There are two sets of regulations, one covering most public authorities
and the other covering private, voluntary and all other public authority
employers. These regulations are nearly identical, and the minor
differences that an employer will need to take into account are
described in this guidance.

### **For most public authority employers**

If employer is listed in Schedule 2 to the Equality Act 2010 (Specific
Duties and Public Authorities) Regulations 2017, and has a headcount of
250 or more on the snapshot date of 31 March of a given year, they must
report and publish their gender pay gap information by 30 March of the
following year.

#### **Public authority employers include:**

-   most government departments (including their executive agencies)

-   the armed forces

-   local authorities

-   NHS bodies

-   universities

-   most schools, including academies and multi-academy trusts (except
    private and independent schools)

### **For private, voluntary, and all other public authority employers**

Private, voluntary, and all other public authority employers must follow
the Equality Act 2010 (Specific Duties and Public Authorities)
Regulations 2017. This includes public authority employers which are not
listed in Schedule 19 to the Equality Act 2010.

For these employers, if they have a headcount of 250 or more on the
snapshot date of 5 April of a given year they must report and publish
your gender pay gap information and written statement by 4 April of the
following year

#### **Private, voluntary, and public authority employers covered by these regulations commonly include:**

-   private limited companies

-   private limited liability partnerships

-   charities

-   independent and private schools

## **What gender pay gap figures are reported**

You must calculate, report and publish these gender pay gap figures:

1.  percentage of men and women in each hourly pay quarter

2.  mean (average) gender pay gap using hourly pay

3.  median gender pay gap using hourly pay

4.  percentage of men and women receiving bonus pay

5.  mean (average) gender pay gap using bonus pay

6.  median gender pay gap using bonus pay

There is more detail on the government website that explains how this
data is gathered and reported.

# Data collection

The initial data was from tidytuesday 28 June 2022
(<https://github.com/rfordatascience/tidytuesday>).

The source of this data was the ‘Gender pay gap service’
(<https://gender-pay-gap.service.gov.uk/>).

From this website CSV files were imported into R and joined together to
create an up to date data set.

Code for this was written into a function called download_paygap() and
saved in ‘\~/script/functions’.

# Data cleaning

Initial data cleaning was performed using code from tidytuesday. This
code created clean and consistent variable names, ensured dates were in
the correct format, and tidied up employer names.

Additional cleaning resulted in conversion of employer_size into an
ordinal factor and ensured that missing data was represented explicitly
using a value of NA. There were also some duplicate observations for a
small number of employers who had submitted \>1 entry for a time period.
Only the most recent entry was retained in these cases.

# Data transformation

There was no clear ‘Year’ variable in the data. There was the due date
and date submitted, but from looking into the data collection process,
there was another date not recorded. This was referred to as the
‘snapshot date’ for data collection. This date is 12 months prior to the
due date. Therefore a new variable ‘year’ was derived by subtracting 1
year from the due date and then extracting the year from this. From
initial exploration it appeared that the ‘submitted after the deadline’
variable may be inaccurate. A new variable was created called
‘late_submit’ which calculated the difference between due date and
submission date. If the submission date was after the due date then this
was assigned TRUE.

# Data tidying

Although now cleaned, this data could not be described as tidy (Hadley
Wickham).

Convert to long. Convert to single type data.
