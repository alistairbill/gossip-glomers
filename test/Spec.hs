-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Data.Aeson
import Data.Data (Data)
import Maelstrom.Core
import Test.Hspec

newtype WithField = WithField
  { snakeCase :: Text
  }
  deriving stock (Generic, Data, Show, Eq)
  deriving (ToJSON, FromJSON) via MessagePayload WithField

data WithoutField = WithoutField
  deriving stock (Generic, Data, Show, Eq)
  deriving (ToJSON, FromJSON) via MessagePayload WithoutField

main :: IO ()
main = hspec $ do
  let withField = WithField "hello"
      withFieldText = "{\"snake_case\":\"hello\",\"type\":\"with_field\"}"
      withoutFieldText = "{\"type\":\"without_field\"}"
  describe "Message" $ do
    it "encodes messages with fields correctly" $ do
      encode
        ( Message
            { src = NodeId "src",
              dest = NodeId "dest",
              body =
                Body
                  { address =
                      Address
                        { msgId = Just (MessageId 1),
                          inReplyTo = Nothing
                        },
                    payload = withField
                  }
            } ::
            Message Local WithField
        )
        `shouldBe` "{\"src\":\"src\",\"dest\":\"dest\",\"body\":{\"msg_id\":1,\"snake_case\":\"hello\",\"type\":\"with_field\"}}"
    it "encodes messages without fields correctly" $ do
      encode
        ( Message
            { src = NodeId "src",
              dest = NodeId "dest",
              body =
                Body
                  { address =
                      Address
                        { msgId = Just (MessageId 1),
                          inReplyTo = Nothing
                        },
                    payload = WithoutField
                  }
            } ::
            Message Local WithoutField
        )
        `shouldBe` "{\"src\":\"src\",\"dest\":\"dest\",\"body\":{\"msg_id\":1,\"type\":\"without_field\"}}"
    it "decodes messages without fields correctly" $ do
      decode "{\"src\":\"src\",\"dest\":\"dest\",\"body\":{\"msg_id\":1,\"type\":\"without_field\"}}"
        `shouldBe` ( Just
                       Message
                         { src = NodeId "src",
                           dest = NodeId "dest",
                           body =
                             Body
                               { address =
                                   Address
                                     { msgId = Just (MessageId 1),
                                       inReplyTo = Nothing
                                     },
                                 payload = WithoutField
                               }
                         } ::
                       Maybe (Message Remote WithoutField)
                   )
    it "decodes messages without fields correctly" $ do
      decode "{\"src\":\"c0\",\"dest\":\"n1\",\"body\":{\"type\":\"without_field\"}}"
        `shouldBe` ( Just
                       Message
                         { src = NodeId "c0",
                           dest = NodeId "n1",
                           body =
                             Body
                               { address =
                                   Address
                                     { msgId = Nothing,
                                       inReplyTo = Nothing
                                     },
                                 payload = WithoutField
                               }
                         } ::
                       Maybe (Message Remote WithoutField)
                   )
  describe "MessagePayload" $ do
    it "encodes payloads with fields correctly" $ do
      encode withField `shouldBe` withFieldText
    it "encodes payloads without fields correctly" $ do
      encode WithoutField `shouldBe` withoutFieldText
    it "decodes payloads with fields correctly" $ do
      decode withFieldText `shouldBe` Just withField
    it "decodes payloads without fields correctly" $ do
      decode withoutFieldText `shouldBe` Just WithoutField
  describe "Body" $ do
    it "encodes body with field correctly" $ do
      let body =
            Body
              { address = Address {msgId = Just (MessageId 1), inReplyTo = Nothing},
                payload = withField
              }
      encode body `shouldBe` "{\"msg_id\":1,\"snake_case\":\"hello\",\"type\":\"with_field\"}"
    it "encodes body without field correctly" $ do
      let body =
            Body
              { address = Address {msgId = Just (MessageId 2), inReplyTo = Just (MessageId 1)},
                payload = WithoutField
              }
      encode body `shouldBe` "{\"in_reply_to\":1,\"msg_id\":2,\"type\":\"without_field\"}"
    it "decodes body with field correctly" $ do
      let body =
            Body
              { address = Address {msgId = Just (MessageId 1), inReplyTo = Nothing},
                payload = withField
              }
      decode "{\"msg_id\":1,\"type\":\"with_field\",\"snake_case\":\"hello\"}" `shouldBe` Just body
    it "decodes body without field correctly" $ do
      let body =
            Body
              { address = Address {msgId = Just (MessageId 2), inReplyTo = Nothing},
                payload = WithoutField
              }
      encode body `shouldBe` "{\"msg_id\":2,\"type\":\"without_field\"}"