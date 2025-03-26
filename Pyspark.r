from pyspark.sql import SparkSession
from pyspark.sql.functions import col, udf
from pyspark.sql.types import IntegerType

# Create Spark session
spark = SparkSession.builder \
    .appName("Count Odd Numbers") \
    .getOrCreate()

# Define the CSV file path
csv_path = "Odd_Num.csv"  # Replace with the actual file path

# Read the CSV file
df = spark.read.csv(csv_path, header=True, inferSchema=True)

# Define UDF to check if a number is odd
def is_odd(number):
    return 1 if number % 2 != 0 else 0

# Register UDF
is_odd_udf = udf(is_odd, IntegerType())

# Add a new column to indicate odd numbers
df = df.withColumn("is_odd", is_odd_udf(col("number")))

# Count the number of odd numbers
odd_count = df.agg({"is_odd": "sum"}).collect()[0][0]

print(f"Count of odd numbers: {odd_count}")

# Stop Spark session
spark.stop()
