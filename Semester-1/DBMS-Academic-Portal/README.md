# Academic Portal in MySQL

**Group project**, DBMS, Semester 1. A relational database and role-based workflow modelled on an AIMS-style academic portal.

## Schema

Tables include `batches`, `students`, `faculty`, `courses`, `course_prereqs`, `timetable_slots`, `offerings`, `offering_allowed_batches`, `registrations`, `credits_earned`, `tickets` and `ticket_actions`.

## Features

- Foreign keys and constraints across the schema, with course prerequisites and timetable slots.
- **Stored procedures** for uploading timetable slots, offering a course, registering for a course, and acting on a ticket.
- A **ticket approval workflow** that moves a request through faculty, advisor and dean stages, with each stage enforced in the procedure.
- **Role-based access** using MySQL users and `GRANT` privileges, for example a student user limited to selecting courses and inserting registrations.
- Sample data and queries demonstrating each module.

## File

`DBMS_Academic_Portal.pdf` has the full SQL and outputs. The database user in it is a throwaway local-development account.
