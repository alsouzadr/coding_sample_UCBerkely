-- This is a sample of a SQL query I did to pull a sample of Spotify users to send a survey. I have changed the names of table and other confidential and proprietary information


CREATE TEMPORARY FUNCTION XXXXX_DECRYPT(ciphertext BYTES, keyset BYTES) RETURNS BYTES AS (
   SAFE.AEAD.DECRYPT_BYTES(keyset, SUBSTR(ciphertext, 5), FROM_HEX(""))
 );
 CREATE TEMPORARY FUNCTION XXXXXX_DECRYPT_STRING(ciphertext BYTES, keyset BYTES) RETURNS STRING AS (
   CAST(XXXXXX_DECRYPT(ciphertext, keyset) AS STRING)
 );
CREATE TEMPORARY FUNCTION
  AGE_BUCKETER(age INT64) RETURNS STRING AS (
    CASE
      WHEN age BETWEEN 0 AND 18 THEN '0-18'
      WHEN age BETWEEN 19 AND 24 THEN '19-24'
      WHEN age BETWEEN 25 AND 29 THEN '25-29'
      WHEN age BETWEEN 30 AND 34 THEN '30-34'
      WHEN age BETWEEN 35 AND 44 THEN '35-44'
      WHEN age BETWEEN 45 AND 54 THEN '45-54'
      WHEN age >= 55 THEN '55+'
      ELSE 'unknown'
    END
  );
CREATE TEMPORARY FUNCTION
  EMAIL_IS_MEDIA(email STRING) RETURNS BOOL AS (
  LOWER(email) LIKE '%@ap.org'
  OR LOWER(email) LIKE '%@xxxxxxxx' -- this is an actual domain or email
  OR LOWER(email) LIKE '%@xxxxxxxx'
  OR LOWER(email) LIKE '%@xxxxxxxx'
  OR LOWER(email) LIKE '%@xxxxxxxx'
  OR LOWER(email) LIKE '%@xxxxxxxx'
  OR LOWER(email) LIKE '%@xxxxxxxx'
  OR LOWER(email) LIKE '%@xxxxxxxx'
  OR LOWER(email) LIKE '%@ xxxxxxxx'
  OR LOWER(email) LIKE '%@xxxxxxxx'
  OR LOWER(email) LIKE '%@xxxxxxxx'
);
CREATE TEMPORARY FUNCTION -- This excludes employees of certain companies and business
  EVER_EMPLOYEE(product STRING) RETURNS BOOL AS (
  LOWER(product) LIKE "%xxx-employee%"
  OR LOWER(product) LIKE "%xxx_employee%"
  OR LOWER(product) LIKE "%xxx-employee%"
  OR LOWER(product) LIKE "%xxx_employee%"
  OR LOWER(product) LIKE "%xxx-employee%"
  OR LOWER(product) LIKE "%xxx_employee%"
);
WITH
employee_userids AS (
    SELECT DISTINCT
      user_id
    FROM
      `table_with_demographics`
      , UNNEST(product.product_history) AS p
    WHERE
      EVER_EMPLOYEE(p.reporting_product)
)
, context_streamshare AS (
    SELECT
      user_id
      , CAST(10 * SUM(c.streamshare) AS INT64) AS programmed_streamshare
    FROM
      `table_with_streamshare`
      , UNNEST(context) AS c
    WHERE
      c.is_programmed
    GROUP BY
      user_id
)
, valid_users AS (
    SELECT
      user_id
      , email
      , us.demographics.gender AS gender
      , AGE_BUCKETER(us.demographics.age) AS age_bucket
      , us.product.is_subscriber AS is_premium
      , us.product.reporting_country AS country
      , coalesce(programmed_streamshare,0) as programmed_streamshare
    FROM
      `table_with_demographics` AS us
      LEFT JOIN context_streamshare USING(user_id)
      JOIN `table_with_emails` USING(user_id)
      LEFT JOIN `table_with_opted_out` USING(user_id)
    WHERE
      -- Account at least 6 months old and currently MAU (update date to reflect 'today' when re-run)
      DATE_DIFF(DATE(2020, 5, 06), SAFE.PARSE_DATE('%Y-%m-%d', registration.date), day) > 6 * 30
      AND activity.is_mau
      -- Defined gender field and at least 16yo
      AND demographics.gender != 'unknown'
      AND demographics.age >= 16
      -- Limit to countries of interest for now
      AND product.reporting_country IN ("AU")
      -- Exclude recently surveyed (update date to reflect 'today' when re-run)
      AND COALESCE(DATE_DIFF(DATE(2020, 5, 06), last_researchsurvey_date, day) > 6 * 30, True)
      -- Exclude anybody that ever had an employee product
      AND user_id NOT IN (SELECT * FROM employee_userids)
      -- Exclude people who opted out
      AND NOT email_optout AND ok_to_email
)
, users_with_email AS ( -- to decrypt the emails so your EMAIL_IS_MEDIA works. Not sure this is the best way to do that.
    SELECT
      user_id
      , XXXXX_DECRYPT_STRING(email, principalPipelineKey) AS email
      , gender
      , age_bucket
      , is_premium
      , country
      , programmed_streamshare
    FROM
      valid_users
      JOIN `table_with_unlock_keys` ON user_id = userId
    WHERE
      NOT EMAIL_IS_MEDIA(SPOTIFY_DECRYPT_STRING(email, principalPipelineKey))
      AND SPOTIFY_DECRYPT_STRING(email, principalPipelineKey)  NOT LIKE ''
)
, ranked_users AS (
    SELECT
      user_id
      , email
      , gender
      , age_bucket
      , is_premium
      , country
      , programmed_streamshare
      -- this will randomly, evenly assign numbers 1-10000 to users in each segment
      -- which we can threshold to get a stratified subsample
      , NTILE(10000) OVER(
        PARTITION BY gender, age_bucket, is_premium, country, programmed_streamshare
        ORDER BY FARM_FINGERPRINT(CONCAT(user_id, "vof_survey"))
      ) AS subsample_user_value
    FROM
      users_with_email
)
, sampled_users AS (
  SELECT
    user_id
    , email
    , gender
    , age_bucket
    , is_premium
    , country
    , programmed_streamshare
  FROM
    ranked_users
  WHERE
    -- adjust this to get a bigger sample. setting to <=1 will give us 1 out of 10,000.
    -- setting to <=500 will give us 500 out of 10,000 (5% of valid)
    subsample_user_value <= 200
)
-- for all artists a user has streamed, but them into buckets for overall
-- artist popularity ("overall_popularity") and within-user ranking ("user_popularity")
-- also filter out "do not recommend" artists
, all_user_artists AS (
  SELECT
    user_id
    , email
    , gender
    , age_bucket
    , is_premium
    , country
    , programmed_streamshare
    , artist_gid
    , artist_name
    , CASE
      WHEN tier IN (0, 1) THEN "high"
      WHEN tier IN (2, 3, 4, 5) THEN "mid"
      ELSE "low"
    END AS overall_popularity
    , NTILE(20) OVER(
      PARTITION BY user_id
      ORDER BY value_float DESC
    ) AS user_popularity
    , value_float AS prob_follow
  FROM
    `table_with_listening_models`
    JOIN `table_with_artists_info` USING(artist_gid)
    , UNNEST(model_feature)
    JOIN sampled_users USING(user_id)
  WHERE
    name = 'prob_follow'
    AND NOT do_not_recommend
    AND NOT COALESCE(is_non_artist, False)
)
-- rank artists randomly within each bucket ("_seed_in_bucket")
-- limit to only selecting artists from top 2 user popularite "buckets"
, artists_seeded_within_bucket AS (
  SELECT
    user_id
    , email
    , gender
    , age_bucket
    , is_premium
    , country
    , programmed_streamshare
    , artist_gid
    , artist_name
    , user_popularity
    , prob_follow
    , ROW_NUMBER() OVER(
        PARTITION BY
          user_id,
          overall_popularity,
          CASE WHEN user_popularity > 14 THEN 14 END
        ORDER BY
          FARM_FINGERPRINT(CONCAT(artist_gid, "VoF_survey")) DESC
    ) AS _seed_in_bucket
  FROM
    all_user_artists
  WHERE
    user_popularity <= 2 -- Artist must be in top 10% of users most favorite
    OR
    -- or be a popular artist in their bottom 10%-30%
    (user_popularity BETWEEN 14 AND 18 AND overall_popularity = "high")
)
-- using the within-bucket rank for each artist, rank artists to be selected
-- using _seed_by_user
, artists_seeded_within_user AS (
  SELECT
    user_id
    , email
    , gender
    , age_bucket
    , is_premium
    , country
    , programmed_streamshare
    , artist_gid 
    , artist_name
    , prob_follow
    , user_popularity
    , ROW_NUMBER() OVER(
      PARTITION BY user_id
      ORDER BY _seed_in_bucket ASC
    ) AS _seed_by_user
  FROM
    artists_seeded_within_bucket
  WHERE
    -- only keep two artists from the "popular music this user dislikes" bucket
    (user_popularity >= 14 AND _seed_in_bucket <= 2)
    OR
    (user_popularity < 14 AND _seed_in_bucket <= 15)
)
, stratified_artist_sample AS (
  SELECT
    user_id
    , email
    , gender
    , age_bucket
    , is_premium
    , country
    , programmed_streamshare
    , artist_gid 
    , artist_name
    , user_popularity
    , ROW_NUMBER() OVER(
      PARTITION BY user_id
      ORDER BY prob_follow DESC
    ) AS prob_follow_rank
  FROM
    artists_seeded_within_user
  WHERE
    _seed_by_user <= 15
)
SELECT
  user_id
  , email
  , gender
  , age_bucket
  , is_premium
  , country
  , programmed_streamshare
  , MAX(CASE WHEN prob_follow_rank = 1 THEN artist_name END) AS a1
  , MAX(CASE WHEN prob_follow_rank = 2 THEN artist_name END) AS a2
  , MAX(CASE WHEN prob_follow_rank = 3 THEN artist_name END) AS a3
  , MAX(CASE WHEN prob_follow_rank = 4 THEN artist_name END) AS a4
  , MAX(CASE WHEN prob_follow_rank = 5 THEN artist_name END) AS a5
  , MAX(CASE WHEN prob_follow_rank = 6 THEN artist_name END) AS a6
  , MAX(CASE WHEN prob_follow_rank = 7 THEN artist_name END) AS a7
  , MAX(CASE WHEN prob_follow_rank = 8 THEN artist_name END) AS a8
  , MAX(CASE WHEN prob_follow_rank = 9 THEN artist_name END) AS a9
  , MAX(CASE WHEN prob_follow_rank = 10 THEN artist_name END) AS a10
  , MAX(CASE WHEN prob_follow_rank = 11 THEN artist_name END) AS a11
  , MAX(CASE WHEN prob_follow_rank = 12 THEN artist_name END) AS a12
  , MAX(CASE WHEN prob_follow_rank = 13 THEN artist_name END) AS a13
  , MAX(CASE WHEN prob_follow_rank = 14 THEN artist_name END) AS a14
  , MAX(CASE WHEN prob_follow_rank = 15 THEN artist_name END) AS a15
  , MAX(CASE WHEN prob_follow_rank = 1 THEN artist_gid END) AS a1a
  , MAX(CASE WHEN prob_follow_rank = 2 THEN artist_gid END) AS a2a
  , MAX(CASE WHEN prob_follow_rank = 3 THEN artist_gid END) AS a3a
  , MAX(CASE WHEN prob_follow_rank = 4 THEN artist_gid END) AS a4a
  , MAX(CASE WHEN prob_follow_rank = 5 THEN artist_gid END) AS a5a
  , MAX(CASE WHEN prob_follow_rank = 6 THEN artist_gid END) AS a6a
  , MAX(CASE WHEN prob_follow_rank = 7 THEN artist_gid END) AS a7a
  , MAX(CASE WHEN prob_follow_rank = 8 THEN artist_gid END) AS a8a
  , MAX(CASE WHEN prob_follow_rank = 9 THEN artist_gid END) AS a9a
  , MAX(CASE WHEN prob_follow_rank = 10 THEN artist_gid END) AS a10a
  , MAX(CASE WHEN prob_follow_rank = 11 THEN artist_gid END) AS a11a
  , MAX(CASE WHEN prob_follow_rank = 12 THEN artist_gid END) AS a12a
  , MAX(CASE WHEN prob_follow_rank = 13 THEN artist_gid END) AS a13a
  , MAX(CASE WHEN prob_follow_rank = 14 THEN artist_gid END) AS a14a
  , MAX(CASE WHEN prob_follow_rank = 15 THEN artist_gid END) AS a15a
FROM
  stratified_artist_sample
WHERE
  (prob_follow_rank <= 13 AND user_popularity <=2)
  OR
  (prob_follow_rank > 13 AND user_popularity > 10)
GROUP BY
  user_id
  , email
  , gender
  , age_bucket
  , is_premium
  , country
  , programmed_streamshare
HAVING
  a15 IS NOT NULL
  AND a14 IS NOT NULL
  AND a13 IS NOT NULL
  AND a12 IS NOT NULL
  AND a11 IS NOT NULL
  AND a10 IS NOT NULL
  AND a9 IS NOT NULL
  AND a8 IS NOT NULL
  AND a7 IS NOT NULL
  AND a6 IS NOT NULL
  AND a5 IS NOT NULL
  AND a4 IS NOT NULL
  AND a3 IS NOT NULL
  AND a2 IS NOT NULL
  AND a1 IS NOT NULL
