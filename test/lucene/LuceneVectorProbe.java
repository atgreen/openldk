// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.nio.file.*;
import java.util.*;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.*;
import org.apache.lucene.index.*;
import org.apache.lucene.store.*;
import org.apache.lucene.search.*;

public class LuceneVectorProbe {
    static void run(Directory directory, VectorSimilarityFunction similarity) throws Exception {
        float[][] vectors = {{1,0,0}, {.9f,.1f,0}, {0,1,0}, {.8f,.2f,0}};
        try (IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(new StandardAnalyzer()))) {
            for (int i = 0; i < vectors.length; i++) {
                Document document = new Document();
                document.add(new StringField("id", Integer.toString(i), Field.Store.YES));
                document.add(new KnnFloatVectorField("v", vectors[i], similarity));
                writer.addDocument(document);
            }
        }
        try (DirectoryReader reader = DirectoryReader.open(directory)) {
            IndexSearcher searcher = new IndexSearcher(reader);
            TopDocs top = searcher.search(new KnnFloatVectorQuery("v", new float[]{1,0,0}, 2), 2);
            if (searcher.count(new MatchAllDocsQuery()) != 4 || top.scoreDocs.length != 2)
                throw new AssertionError("wrong document/neighbor count: " + directory + " " + similarity);
            Set<String> ids = new TreeSet<>();
            for (ScoreDoc hit : top.scoreDocs) {
                if (!Float.isFinite(hit.score)) throw new AssertionError("non-finite score");
                ids.add(searcher.storedFields().document(hit.doc).get("id"));
            }
            if (!ids.equals(Set.of("0", "1"))) throw new AssertionError("wrong neighbors: " + ids);
            System.out.println(directory.getClass().getSimpleName() + " " + similarity + " neighbors=" + ids);
        }
    }
    public static void main(String[] args) throws Exception {
        Path root = Path.of(args[0]);
        for (VectorSimilarityFunction similarity : VectorSimilarityFunction.values()) {
            Path path = Files.createDirectory(root.resolve(similarity.name()));
            try (Directory directory = FSDirectory.open(path)) { run(directory, similarity); }
            try (Directory directory = new ByteBuffersDirectory()) { run(directory, similarity); }
        }
        System.out.println("Lucene vector searches passed");
    }
}
