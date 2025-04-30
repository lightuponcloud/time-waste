import asyncio
from playwright.async_api import async_playwright
from bs4 import BeautifulSoup
from urllib.parse import urljoin, urlparse
import aiohttp
import json
from typing import List, Dict, Set
import trafilatura
from sentence_transformers import SentenceTransformer
import numpy as np
from opensearchpy import OpenSearch

class WebScraper:
    def __init__(self, base_url: str, max_pages: int = 5000):
        self.base_url = base_url
        self.domain = urlparse(base_url).netloc
        self.visited_urls: Set[str] = set()
        self.max_pages = max_pages
        self.data = []

    async def initialize_browser(self):
        self.playwright = await async_playwright().start()
        self.browser = await self.playwright.chromium.launch(headless=True)
        self.context = await self.browser.new_context(
            viewport={'width': 1920, 'height': 1080},
            user_agent='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        )

    async def close(self):
        await self.context.close()
        await self.browser.close()
        await self.playwright.stop()

    def is_valid_url(self, url: str) -> bool:
        if not url or not url.startswith('http'):
            return False
        parsed = urlparse(url)
        return parsed.netloc == self.domain

    async def extract_content(self, url: str) -> Dict:
        try:
            page = await self.context.new_page()
            await page.goto(url, wait_until='networkidle')
            content = await page.content()

            # Use trafilatura for main content extraction
            extracted_text = trafilatura.extract(content)

            # Use BeautifulSoup for structured data extraction
            soup = BeautifulSoup(content, 'html.parser')

            # Extract specific metadata
            title = soup.title.string if soup.title else ''
            meta_description = soup.find('meta', {'name': 'description'})
            description = meta_description['content'] if meta_description else ''

            # Close the page to free resources
            await page.close()

            return {
                'url': url,
                'title': title,
                'description': description,
                'content': extracted_text,
                'last_updated': None  # Will be filled by the content diff checker
            }
        except Exception as e:
            print(f"Error processing {url}: {str(e)}")
            return None

    async def get_all_links(self, url: str) -> List[str]:
        try:
            page = await self.context.new_page()
            await page.goto(url, wait_until='networkidle')
            links = await page.evaluate('''() => {
                return Array.from(document.links).map(link => link.href);
            }''')
            await page.close()
            return [link for link in links if self.is_valid_url(link)]
        except Exception as e:
            print(f"Error getting links from {url}: {str(e)}")
            return []

    async def crawl(self):
        await self.initialize_browser()

        urls_to_visit = [self.base_url]

        while urls_to_visit and len(self.visited_urls) < self.max_pages:
            current_url = urls_to_visit.pop(0)

            if current_url in self.visited_urls:
                continue

            self.visited_urls.add(current_url)

            content = await self.extract_content(current_url)
            if content:
                self.data.append(content)

            new_links = await self.get_all_links(current_url)
            urls_to_visit.extend([url for url in new_links if url not in self.visited_urls])

        await self.close()

class ContentProcessor:
    def __init__(self):
        self.model = SentenceTransformer('all-MiniLM-L6-v2')
        self.chunk_size = 512

    def chunk_text(self, text: str) -> List[str]:
        words = text.split()
        chunks = []
        current_chunk = []
        current_length = 0

        for word in words:
            if current_length + len(word) > self.chunk_size:
                chunks.append(' '.join(current_chunk))
                current_chunk = [word]
                current_length = len(word)
            else:
                current_chunk.append(word)
                current_length += len(word)

        if current_chunk:
            chunks.append(' '.join(current_chunk))

        return chunks

    def process_content(self, data: List[Dict]) -> List[Dict]:
        processed_data = []

        for item in data:
            chunks = self.chunk_text(item['content'])
            embeddings = self.model.encode(chunks)

            for chunk, embedding in zip(chunks, embeddings):
                processed_data.append({
                    'url': item['url'],
                    'title': item['title'],
                    'chunk': chunk,
                    'embedding': embedding.tolist(),
                    'last_updated': item['last_updated']
                })

        return processed_data

class OpenSearchManager:
    def __init__(self, host: str, port: int, index_name: str):
        self.client = OpenSearch(
            hosts=[{'host': host, 'port': port}],
            http_auth=('admin', 'admin'),
            use_ssl=True,
            verify_certs=False
        )
        self.index_name = index_name

    def create_index(self):
        index_body = {
            'mappings': {
                'properties': {
                    'url': {'type': 'keyword'},
                    'title': {'type': 'text'},
                    'chunk': {'type': 'text'},
                    'embedding': {'type': 'dense_vector', 'dims': 384},
                    'last_updated': {'type': 'date'}
                }
            }
        }

        self.client.indices.create(index=self.index_name, body=index_body)

    def index_documents(self, documents: List[Dict]):
        for doc in documents:
            self.client.index(
                index=self.index_name,
                body=doc,
                id=f"{doc['url']}_{hash(doc['chunk'])}"
            )

async def main():
    # Example usage
    scraper = WebScraper('https://example-university.edu')
    await scraper.crawl()

    processor = ContentProcessor()
    processed_data = processor.process_content(scraper.data)

    opensearch_manager = OpenSearchManager('localhost', 9200, 'university_content')
    opensearch_manager.create_index()
    opensearch_manager.index_documents(processed_data)

if __name__ == "__main__":
    asyncio.run(main())import asyncio
from playwright.async_api import async_playwright
from bs4 import BeautifulSoup
from urllib.parse import urljoin, urlparse
import aiohttp
import json
from typing import List, Dict, Set
import trafilatura
from sentence_transformers import SentenceTransformer
import numpy as np
from opensearchpy import OpenSearch

class WebScraper:
    def __init__(self, base_url: str, max_pages: int = 5000):
        self.base_url = base_url
        self.domain = urlparse(base_url).netloc
        self.visited_urls: Set[str] = set()
        self.max_pages = max_pages
        self.data = []

    async def initialize_browser(self):
        self.playwright = await async_playwright().start()
        self.browser = await self.playwright.chromium.launch(headless=True)
        self.context = await self.browser.new_context(
            viewport={'width': 1920, 'height': 1080},
            user_agent='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        )

    async def close(self):
        await self.context.close()
        await self.browser.close()
        await self.playwright.stop()

    def is_valid_url(self, url: str) -> bool:
        if not url or not url.startswith('http'):
            return False
        parsed = urlparse(url)
        return parsed.netloc == self.domain

    async def extract_content(self, url: str) -> Dict:
        try:
            page = await self.context.new_page()
            await page.goto(url, wait_until='networkidle')
            content = await page.content()

            # Use trafilatura for main content extraction
            extracted_text = trafilatura.extract(content)

            # Use BeautifulSoup for structured data extraction
            soup = BeautifulSoup(content, 'html.parser')

            # Extract specific metadata
            title = soup.title.string if soup.title else ''
            meta_description = soup.find('meta', {'name': 'description'})
            description = meta_description['content'] if meta_description else ''

            # Close the page to free resources
            await page.close()

            return {
                'url': url,
                'title': title,
                'description': description,
                'content': extracted_text,
                'last_updated': None  # Will be filled by the content diff checker
            }
        except Exception as e:
            print(f"Error processing {url}: {str(e)}")
            return None

    async def get_all_links(self, url: str) -> List[str]:
        try:
            page = await self.context.new_page()
            await page.goto(url, wait_until='networkidle')
            links = await page.evaluate('''() => {
                return Array.from(document.links).map(link => link.href);
            }''')
            await page.close()
            return [link for link in links if self.is_valid_url(link)]
        except Exception as e:
            print(f"Error getting links from {url}: {str(e)}")
            return []

    async def crawl(self):
        await self.initialize_browser()

        urls_to_visit = [self.base_url]

        while urls_to_visit and len(self.visited_urls) < self.max_pages:
            current_url = urls_to_visit.pop(0)

            if current_url in self.visited_urls:
                continue

            self.visited_urls.add(current_url)

            content = await self.extract_content(current_url)
            if content:
                self.data.append(content)

            new_links = await self.get_all_links(current_url)
            urls_to_visit.extend([url for url in new_links if url not in self.visited_urls])

        await self.close()

class ContentProcessor:
    def __init__(self):
        self.model = SentenceTransformer('all-MiniLM-L6-v2')
        self.chunk_size = 512

    def chunk_text(self, text: str) -> List[str]:
        words = text.split()
        chunks = []
        current_chunk = []
        current_length = 0

        for word in words:
            if current_length + len(word) > self.chunk_size:
                chunks.append(' '.join(current_chunk))
                current_chunk = [word]
                current_length = len(word)
            else:
                current_chunk.append(word)
                current_length += len(word)

        if current_chunk:
            chunks.append(' '.join(current_chunk))

        return chunks

    def process_content(self, data: List[Dict]) -> List[Dict]:
        processed_data = []

        for item in data:
            chunks = self.chunk_text(item['content'])
            embeddings = self.model.encode(chunks)

            for chunk, embedding in zip(chunks, embeddings):
                processed_data.append({
                    'url': item['url'],
                    'title': item['title'],
                    'chunk': chunk,
                    'embedding': embedding.tolist(),
                    'last_updated': item['last_updated']
                })

        return processed_data

class OpenSearchManager:
    def __init__(self, host: str, port: int, index_name: str):
        self.client = OpenSearch(
            hosts=[{'host': host, 'port': port}],
            http_auth=('admin', 'admin'),
            use_ssl=True,
            verify_certs=False
        )
        self.index_name = index_name

    def create_index(self):
        index_body = {
            'mappings': {
                'properties': {
                    'url': {'type': 'keyword'},
                    'title': {'type': 'text'},
                    'chunk': {'type': 'text'},
                    'embedding': {'type': 'dense_vector', 'dims': 384},
                    'last_updated': {'type': 'date'}
                }
            }
        }

        self.client.indices.create(index=self.index_name, body=index_body)

    def index_documents(self, documents: List[Dict]):
        for doc in documents:
            self.client.index(
                index=self.index_name,
                body=doc,
                id=f"{doc['url']}_{hash(doc['chunk'])}"
            )

async def main():
    # Example usage
    scraper = WebScraper('https://example-university.edu')
    await scraper.crawl()

    processor = ContentProcessor()
    processed_data = processor.process_content(scraper.data)

    opensearch_manager = OpenSearchManager('localhost', 9200, 'university_content')
    opensearch_manager.create_index()
    opensearch_manager.index_documents(processed_data)

if __name__ == "__main__":
    asyncio.run(main())

