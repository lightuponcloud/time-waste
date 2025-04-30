The Task
========
> Design a web scraping solution using Playwright
> Implement content processing with Trafilatura and embedding generation
> Set up storage and retrieval using OpenSearch
> Create cost optimization strategies

I provided the following.

CDK Project
===========
A well-structured AWS CDK project defining infrastructure-as-code for a cloud-native web application. 
CDK code showed a deployment pipeline with a development environment.

Core Architecture Components were:
- Frontend Layer
- API Gateway Layer
- Application Services ( ECS FarGate with specific CPU and memory allocations )
- Data Layer ( RDS )
- Authentication ( AWS Cognito )
- Network Infrastructure: Custom VPC defined in NetworkStack, Route53 for DNS management
- Serverless Components ( Lambda functions working alongside Fargate services, API Gateway for RESTful API management )
- OpenSearch cluster for Retrieval Augmented Generation
- S3 for file storage
- Security Features ( Secret management for database credentials )

Also I provided [sample web scraper script](assignment.py).